/* This file is part of RTags (http://rtags.net).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "DumpThread.h"
#include <rct/Connection.h>
#include "RTagsClang.h"
#include "Server.h"

DumpThread::DumpThread(const std::shared_ptr<QueryMessage> &queryMessage, const Source &source, const std::shared_ptr<Connection> &conn)
    : Thread(), mQueryFlags(queryMessage->flags()), mSource(source), mConnection(conn), mIndentLevel(0), mAborted(false)
{
    setAutoDelete(true);
}

static const CXSourceLocation nullLocation = clang_getNullLocation();
static const CXCursor nullCursor = clang_getNullCursor();

CXChildVisitResult DumpThread::visitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    DumpThread *that = reinterpret_cast<DumpThread*>(userData);
    assert(that);
    return that->visit(cursor);
}

CXChildVisitResult DumpThread::visit(const CXCursor &cursor)
{
    if (isAborted())
        return CXChildVisit_Break;
    const Location location = createLocation(cursor);
    if (!location.isNull()) {
        if (mQueryFlags & QueryMessage::DumpCheckIncludes) {
            checkIncludes(location, cursor);
        } else {
            Flags<Location::ToStringFlag> locationFlags;
            if (mQueryFlags & QueryMessage::NoColor)
                locationFlags |= Location::NoColor;

            CXSourceRange range = clang_getCursorExtent(cursor);
            CXSourceLocation rangeEnd = clang_getRangeEnd(range);
            unsigned int endLine, endColumn;
            clang_getPresumedLocation(rangeEnd, 0, &endLine, &endColumn);
            if (!(mQueryFlags & QueryMessage::DumpIncludeHeaders) && location.fileId() != mSource.fileId) {
                return CXChildVisit_Continue;
            }

            String message;
            message.reserve(256);

            if (!(mQueryFlags & QueryMessage::NoContext)) {
                message = location.context(locationFlags);
            }

            if (endLine == location.line()) {
                message += String::format<32>(" // %d-%d, %d: ", location.column(), endColumn, mIndentLevel);
            } else {
                message += String::format<32>(" // %d-%d:%d, %d: ", location.column(), endLine, endColumn, mIndentLevel);
            }
            message += RTags::cursorToString(cursor, RTags::AllCursorToStringFlags);
            message.append(" " + RTags::typeName(cursor));;
            if (clang_getCursorKind(cursor) == CXCursor_VarDecl) {
                const std::shared_ptr<RTags::Auto> autoResolved = RTags::resolveAuto(cursor);
                if (autoResolved && !clang_equalCursors(autoResolved->cursor, nullCursor)) {
                    message += "auto resolves to " + RTags::cursorToString(autoResolved->cursor, RTags::AllCursorToStringFlags);
                }
            }
            CXCursor ref = clang_getCursorReferenced(cursor);
            if (clang_equalCursors(ref, cursor)) {
                message.append("refs self");
            } else if (!clang_equalCursors(ref, nullCursor)) {
                message.append("refs ");
                message.append(RTags::cursorToString(ref, RTags::AllCursorToStringFlags));
            }

            CXCursor canonical = clang_getCanonicalCursor(cursor);
            if (!clang_equalCursors(canonical, cursor) && !clang_equalCursors(canonical, nullCursor)) {
                message.append("canonical ");
                message.append(RTags::cursorToString(canonical, RTags::AllCursorToStringFlags));
            }

            CXCursor specialized = clang_getSpecializedCursorTemplate(cursor);
            if (!clang_equalCursors(specialized, cursor) && !clang_equalCursors(specialized, nullCursor)) {
                message.append("specialized ");
                message.append(RTags::cursorToString(specialized, RTags::AllCursorToStringFlags));
            }

            writeToConnetion(message);
        }
    }
    ++mIndentLevel;
    clang_visitChildren(cursor, DumpThread::visitor, this);
    if (isAborted())
        return CXChildVisit_Break;
    --mIndentLevel;
    return CXChildVisit_Continue;
}

void DumpThread::run()
{
    const auto key = mConnection->disconnected().connect([this](const std::shared_ptr<Connection> &) { abort(); });

    CXIndex index = clang_createIndex(0, 0);
    CXTranslationUnit translationUnit = 0;
    String clangLine;
    RTags::parseTranslationUnit(mSource.sourceFile(), mSource.toCommandLine(Source::Default), translationUnit,
                                index, 0, 0, CXTranslationUnit_DetailedPreprocessingRecord, &clangLine);
    if (!(mQueryFlags & QueryMessage::DumpCheckIncludes))
        writeToConnetion(String::format<128>("Indexed: %s => %s", clangLine.constData(), translationUnit ? "success" : "failure"));
    if (translationUnit) {
        clang_visitChildren(clang_getTranslationUnitCursor(translationUnit), DumpThread::visitor, this);
        clang_disposeTranslationUnit(translationUnit);
    }

    clang_disposeIndex(index);
    mConnection->disconnected().disconnect(key);
    std::weak_ptr<Connection> conn = mConnection;
    if (mQueryFlags & QueryMessage::DumpCheckIncludes) {
        checkIncludes();
    }
    EventLoop::mainEventLoop()->callLater([conn]() {
            if (auto c = conn.lock())
                c->finish();
        });
}

void DumpThread::writeToConnetion(const String &message)
{
    std::weak_ptr<Connection> conn = mConnection;
    EventLoop::mainEventLoop()->callLater([conn, message]() {
            if (auto c = conn.lock()) {
                c->write(message);
            }
        });
}

void DumpThread::handleInclude(const Location &loc, const CXCursor &cursor)
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

void DumpThread::handleReference(const Location &loc, const CXCursor &ref)
{
    const Location refLoc = createLocation(ref);
    if (refLoc.isNull())
        return;

    Dep *dep = mDependencies[loc.fileId()];
    assert(dep);
    Dep *refDep = mDependencies[refLoc.fileId()];
    assert(refDep);
    dep->references.insert(refDep);
    refDep->referencedBy.insert(dep);
}


void DumpThread::checkIncludes(const Location &location, const CXCursor &cursor)
{
    if (clang_getCursorKind(cursor) == CXCursor_InclusionDirective) {
        handleInclude(location, cursor);
    } else {
        const CXCursor ref = clang_getCursorReferenced(cursor);
        if (!clang_equalCursors(ref, cursor) && !clang_equalCursors(ref, nullCursor)) {
            handleReference(location, ref);
        }
    }
}

void DumpThread::checkIncludes()
{
    std::function<bool(Dep *, Dep *, Set<uint32_t> &, int)> validate = [this, &validate](Dep *source, Dep *header, Set<uint32_t> &seen, int depth) {
        // error() << depth << "Checking" << Location::path(source->fileId) << Location::path(header->fileId);
        if (!seen.insert(source->fileId)) {
            // error() << "already seen" << Location::path(source->fileId);
            return false;
        }
        if (source->references.contains(header)) {
            // error() << "Got ref" << Location::path(header->fileId);
            return true;
        }
        for (const auto &child : source->includes) {
            // error() << "Checking child" << Location::path(child.second->fileId);
            if (validate(static_cast<Dep*>(child.second), header, seen, depth + 1)) {
                return true;
            }
        }

        // error() << "Checking" << Location::path(source->fileId) << "doesn't seem to need" << Location::path(header->fileId) << depth;
        return false;
    };
    for (const auto &it : mDependencies) {
        // error() << "got it" << Location::path(it.first);
        for (const auto &dep  : it.second->includes) {
            // error() << "got include" << Location::path(dep.first);
            if (Location::path(it.first).isSystem())
                continue;

            Set<uint32_t> seen;
            if (!validate(it.second, static_cast<Dep*>(dep.second), seen, 0)) {
                writeToConnetion(String::format<128>("%s includes %s for no reason",
                                                     Location::path(it.first).constData(),
                                                     Location::path(dep.second->fileId).constData()));
            }
        }
    }

    for (auto it : mDependencies) {
        delete it.second;
    }
}

