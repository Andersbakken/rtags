/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include "ClangThread.h"

#include <assert.h>
#include <functional>
#include <utility>

#include "rct/Connection.h"
#include "RTags.h"
#include "Location.h"
#include "Project.h"
#include "QueryMessage.h"
#include "clang-c/CXString.h"
#include "clang-c/Index.h"
#include "rct/EventLoop.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Log.h"
#include "rct/Map.h"
#include "rct/Path.h"
#include "rct/SignalSlot.h"
#include "rct/StopWatch.h"

struct Dep : public DependencyNode
{
    Dep(uint32_t f)
        : DependencyNode(f)
    {}
    Hash<uint32_t, Map<Location, Location> > references;
};

ClangThread::ClangThread(const std::shared_ptr<QueryMessage> &queryMessage,
                         const Source &source, const std::shared_ptr<Connection> &conn)
    : Thread(), mQueryMessage(queryMessage), mSource(source),
      mConnection(conn), mIndentLevel(0), mAborted(false)
{
    setAutoDelete(true);
}

ClangThread::~ClangThread()
{
}

CXChildVisitResult ClangThread::visitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    ClangThread *that = reinterpret_cast<ClangThread*>(userData);
    assert(that);
    return that->visit(cursor);
}

CXChildVisitResult ClangThread::visit(const CXCursor &cursor)
{
    if (isAborted())
        return CXChildVisit_Break;
    const Location location = RTags::createLocation(cursor);
    if (!location.isNull()) {
        if (mQueryMessage->flags() & QueryMessage::DumpCheckIncludes) {
            checkIncludes(location, cursor);
        } else {
            Flags<Location::ToStringFlag> locationFlags;
            if (mQueryMessage->flags() & QueryMessage::NoColor)
                locationFlags |= Location::NoColor;

            CXSourceRange range = clang_getCursorExtent(cursor);
            CXSourceLocation rangeEnd = clang_getRangeEnd(range);
            unsigned int endLine, endColumn;
            clang_getPresumedLocation(rangeEnd, nullptr, &endLine, &endColumn);
            if (!(mQueryMessage->flags() & QueryMessage::DumpIncludeHeaders) && location.fileId() != mSource.fileId) {
                return CXChildVisit_Continue;
            }

            String message;
            message.reserve(256);

            if (!(mQueryMessage->flags() & QueryMessage::NoContext)) {
                message = location.context(locationFlags, &mContextCache);
            }

            if (endLine == location.line()) {
                message += String::format<32>(" // %d-%d, %d: ", location.column(), endColumn, mIndentLevel);
            } else {
                message += String::format<32>(" // %d-%d:%d, %d: ", location.column(), endLine, endColumn, mIndentLevel);
            }
            message += RTags::cursorToString(cursor, RTags::AllCursorToStringFlags);
            message.append(" " + RTags::typeName(cursor));;
            if (clang_getCursorKind(cursor) == CXCursor_VarDecl) {
                RTags::Auto autoResolved;
                if (RTags::resolveAuto(cursor, &autoResolved) && RTags::isValid(autoResolved.cursor)) {
                    message += "auto resolves to " + RTags::cursorToString(autoResolved.cursor, RTags::AllCursorToStringFlags);
                }
            }
            auto printCursor = [&message](const CXCursor &c, bool *spec = nullptr) {
                CXCursor canonical = clang_getCanonicalCursor(c);
                if (canonical != c && RTags::isValid(canonical)) {
                    message.append(" canonical ");
                    message.append(RTags::cursorToString(canonical, RTags::AllCursorToStringFlags));
                }

                CXCursor specialized = clang_getSpecializedCursorTemplate(c);
                if (specialized != c && RTags::isValid(specialized)) {
                    message.append(" specialized ");
                    message.append(RTags::cursorToString(specialized, RTags::AllCursorToStringFlags));
                    if (spec)
                        *spec = true;
                } else if (spec) {
                    *spec = false;
                }
            };

            const int argCount = clang_Cursor_getNumArguments(cursor);
            if (argCount != -1) {
                message.append(String::format("arg count: %d ", argCount));
            }

            CXCursor ref = clang_getCursorReferenced(cursor);
            bool refSpecialized = false;
            if (RTags::isValid(ref) && ref != cursor) {
                message.append("refs ");
                message.append(RTags::cursorToString(ref, RTags::AllCursorToStringFlags));
                printCursor(ref, &refSpecialized);
                if (refSpecialized && cursor != CXCursor_DeclRefExpr && cursor != CXCursor_MemberRefExpr)
                    refSpecialized = false;
            }

            printCursor(cursor);

            writeToConnection(message);
            if (refSpecialized) {
                visit(ref);
            }
        }
    }
    const String usr = RTags::usr(cursor);
    if (usr.isEmpty() || mSeen.insert(usr)) {
        ++mIndentLevel;
        clang_visitChildren(cursor, ClangThread::visitor, this);
        if (isAborted())
            return CXChildVisit_Break;
        --mIndentLevel;
    }
    return CXChildVisit_Continue;
}

void ClangThread::run()
{
    StopWatch sw;
    const auto key = mConnection->disconnected().connect([this](const std::shared_ptr<Connection> &) { abort(); });

    String sourceCode = mSource.sourceFile().readAll();
    CXUnsavedFile unsaved = {
        mSource.sourceFile().constData(),
        sourceCode.constData(),
        static_cast<unsigned long>(sourceCode.size())
    };


    std::shared_ptr<RTags::TranslationUnit> translationUnit = RTags::TranslationUnit::create(mSource.sourceFile(),
                                                                                             mSource.toCommandLine(Source::Default),
                                                                                             &unsaved, 1, CXTranslationUnit_DetailedPreprocessingRecord,
                                                                                             false);

    const unsigned long long parseTime = sw.restart();
    warning() << "parseTime" << parseTime;
    {
        if (mQueryMessage->type() == QueryMessage::DumpFile && mQueryMessage->flags() & QueryMessage::DumpCheckIncludes)
            writeToConnection(String::format<128>("Indexed: %s => %s", translationUnit->clangLine.constData(), translationUnit ? "success" : "failure"));

        if (translationUnit) {
            clang_visitChildren(clang_getTranslationUnitCursor(translationUnit->unit), ClangThread::visitor, this);
            if (mQueryMessage->flags() & QueryMessage::DumpCheckIncludes)
                checkIncludes();
        }
    }


    mConnection->disconnected().disconnect(key);
    std::weak_ptr<Connection> conn = mConnection;
    EventLoop::mainEventLoop()->callLater([conn]() {
        if (auto c = conn.lock())
            c->finish();
    });
}

void ClangThread::writeToConnection(const String &message)
{
    std::weak_ptr<Connection> conn = mConnection;
    EventLoop::mainEventLoop()->callLater([conn, message]() {
        if (auto c = conn.lock()) {
            c->write(message);
        }
    });
}

void ClangThread::handleInclude(Location loc, const CXCursor &cursor)
{
    CXFile includedFile = clang_getIncludedFile(cursor);
    if (includedFile) {
        CXStringScope fn = clang_getFileName(includedFile);
        const char *cstr = clang_getCString(fn);
        if (!cstr) {
            clang_disposeString(fn);
            return;
        }
        const Path p = Path::resolved(cstr);
        clang_disposeString(fn);
        const uint32_t fileId = Location::insertFile(p);
        Dep *&source = mDependencies[loc.fileId()];
        if (!source)
            source = new Dep(loc.fileId());
        Dep *&include = mDependencies[fileId];
        if (!include)
            include = new Dep(fileId);
        source->include(include);
    }
}

void ClangThread::handleReference(Location loc, const CXCursor &ref)
{
    if (clang_getCursorKind(ref) == CXCursor_Namespace)
        return;
    const Location refLoc = RTags::createLocation(ref);
    if (refLoc.isNull() || refLoc.fileId() == loc.fileId())
        return;

    Dep *dep = mDependencies[loc.fileId()];
    assert(dep);
    Dep *refDep = mDependencies[refLoc.fileId()];
    assert(refDep);
    auto &refs = dep->references[refDep->fileId];
    refs[loc] = refLoc;
}

void ClangThread::checkIncludes(Location location, const CXCursor &cursor)
{
    if (clang_getCursorKind(cursor) == CXCursor_InclusionDirective) {
        handleInclude(location, cursor);
    } else {
        const CXCursor ref = clang_getCursorReferenced(cursor);
        if (RTags::isValid(cursor) && cursor != ref) {
            handleReference(location, ref);
        }
    }
}

static bool validateHasInclude(uint32_t ref, const Dep *cur, Set<uint32_t> &seen)
{
    assert(ref);
    assert(cur);
    if (cur->includes.contains(ref))
        return true;
    if (!seen.insert(ref))
        return false;
    for (const auto &pair : cur->includes) {
        if (validateHasInclude(ref, static_cast<const Dep*>(pair.second), seen))
            return true;
    }
    return false;
}

static bool validateNeedsInclude(const Dep *source, const Dep *header, Set<uint32_t> &seen)
{
    if (!seen.insert(header->fileId)) {
        // error() << "already seen" << Location::path(source->fileId);
        return false;
    }
    if (source->references.contains(header->fileId)) {
        // error() << "Got ref" << Location::path(header->fileId);
        return true;
    }
    for (const auto &child : header->includes) {
        // error() << "Checking child" << Location::path(child.second->fileId);
        if (validateNeedsInclude(source, static_cast<const Dep*>(child.second), seen)) {
            return true;
        }
    }

    // error() << "Checking" << Location::path(source->fileId) << "doesn't seem to need" << Location::path(header->fileId) << depth;
    return false;
}

void ClangThread::checkIncludes()
{
    for (const auto &it : mDependencies) {
        const Path path = Location::path(it.first);
        if (path.isSystem())
            continue;

        for (const auto &dep  : it.second->includes) {
            Set<uint32_t> seen;
            if (!validateNeedsInclude(it.second, static_cast<Dep*>(dep.second), seen)) {
                writeToConnection(String::format<128>("%s includes %s for no reason",
                                                      path.constData(),
                                                      Location::path(dep.second->fileId).constData()));
            }
        }

        continue; // the rest of this doeesn't really work that well.

        for (const auto &ref : it.second->references) {
            const Path refPath = Location::path(ref.first);
            if (refPath.startsWith("/usr/include/sys/_types/_") || refPath.startsWith("/usr/include/_types/_"))
                continue;
            Set<uint32_t> seen;
            if (!validateHasInclude(ref.first, it.second, seen)) {
                List<String> reasons;
                for (const auto &r : ref.second) {
                    String reason;
                    Log log(&reason);
                    log << r.first << "=>" << r.second;
                    reasons << reason;
                }
                writeToConnection(String::format<128>("%s should include %s (%s)",
                                                      Location::path(it.first).constData(),
                                                      Location::path(ref.first).constData(),
                                                      String::join(reasons, " ").constData()));
                // for (const auto &incs : mDependencies[ref.first]->dependents) {
                //     writeToConnection(String::format<128>("GOT INCLUDER %s:%d", Location::path(incs.first).constData(),
                //                                          incs.first));
                // }
            }
        }
    }

    for (auto it : mDependencies) {
        delete it.second;
    }
}
