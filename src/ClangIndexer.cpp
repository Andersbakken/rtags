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

#define RTAGS_SINGLE_THREAD
#include "ClangIndexer.h"

#include <unistd.h>
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 25)
#include <clang-c/Documentation.h>
#endif

#include "Diagnostic.h"
#include "FileMap.h"
#include "QueryMessage.h"
#include "RClient.h"
#include "rct/Connection.h"
#include "rct/EventLoop.h"
#include "rct/SHA256.h"
#include "Sandbox.h"
#include "RTags.h"
#include "RTagsVersion.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include "Location.h"

static inline void setType(Symbol &symbol, const CXType &type)
{
    symbol.type = type.kind;
    symbol.typeName = RTags::eatString(clang_getTypeSpelling(type));
    const CXType canonical = clang_getCanonicalType(type);
    if (!clang_equalTypes(type, canonical)
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 32)
        && (symbol.typeName == "auto" || type.kind == CXType_Auto)
#endif
        ) {
        symbol.typeName += " => " + RTags::eatString(clang_getTypeSpelling(canonical));
    }
}

static inline void setRange(Symbol &symbol, const CXSourceRange &range, uint16_t *length = nullptr)
{
    CXSourceLocation rangeStart = clang_getRangeStart(range);
    CXSourceLocation rangeEnd = clang_getRangeEnd(range);
    unsigned int startLine, startColumn, endLine, endColumn, startOffset, endOffset;
    clang_getSpellingLocation(rangeStart, nullptr, &startLine, &startColumn, &startOffset);
    clang_getSpellingLocation(rangeEnd, nullptr, &endLine, &endColumn, &endOffset);
    symbol.startLine = startLine;
    symbol.endLine = endLine;
    symbol.startColumn = startColumn;
    symbol.endColumn = endColumn;
    if (length)
        *length = static_cast<uint16_t>(endOffset - startOffset);
}

struct VerboseVisitorUserData {
    int indent;
    String out;
    ClangIndexer *indexer;
};

ClangIndexer::State ClangIndexer::sState = ClangIndexer::NotStarted;
std::mutex ClangIndexer::sStateMutex;
Flags<Server::Option> ClangIndexer::sServerOpts;
ClangIndexer::ClangIndexer(Mode mode)
    : mMode(mode), mCurrentTranslationUnit(String::npos), mLastCursor(clang_getNullCursor()),
      mLastCallExprSymbol(nullptr), mVisitFileResponseMessageFileId(0),
      mVisitFileResponseMessageVisit(0), mParseDuration(0), mVisitDuration(0), mBlocked(0),
      mAllowed(0), mIndexed(1), mVisitFileTimeout(0), mIndexDataMessageTimeout(0),
      mFileIdsQueried(0), mFileIdsQueriedTime(0), mCursorsVisited(0), mLogFile(nullptr),
      mConnection(Connection::create(RClient::NumOptions)), mUnionRecursion(false),
      mFromCache(false), mInTemplateFunction(0)
{
    mConnection->newMessage().connect(std::bind(&ClangIndexer::onMessage, this,
                                                std::placeholders::_1, std::placeholders::_2));
}

ClangIndexer::~ClangIndexer()
{
    if (mLogFile)
        fclose(mLogFile);
}

bool ClangIndexer::exec(const String &data)
{
    {
        std::unique_lock<std::mutex> lock(sStateMutex);
        if (sState == Stopped) {
            sState = NotStarted;
            return true;
        }
        assert(sState == NotStarted);
        sState = Running;
    }
    mFromCache = false;
    mTimer.restart();
    mMacroTokens.clear();
    mUnits.clear();
    mCurrentTranslationUnit = 0;
    mLastCursor = clang_getNullCursor();
    mLastCallExprSymbol = nullptr;
    mLastClass = Location();
    mVisitFileResponseMessageVisit = 0;
    mParseDuration = mVisitDuration = mBlocked = mAllowed = mVisitFileTimeout = 0;
    mIndexDataMessageTimeout = mFileIdsQueried = mFileIdsQueriedTime = mCursorsVisited = 0;
    mIndexed = 1;
    mUnionRecursion = false;
    mScopeStack.clear();
    mLoopStack.clear();
    mParents.clear();
    mTemplateSpecializations.clear();
    mInTemplateFunction = false;
    mIndexDataMessage.clear();
    mTranslationUnits.clear();
    mUnsavedFiles.clear();

    Deserializer deserializer(data);
    uint16_t protocolVersion;
    deserializer >> protocolVersion;
    if (protocolVersion != RTags::DatabaseVersion) {
        error("Wrong protocol %d vs %d", protocolVersion, RTags::DatabaseVersion);
        return false;
    }
    uint64_t id;
    String socketFile;
    Flags<IndexerJob::Flag> indexerJobFlags;
    uint32_t connectTimeout, connectAttempts;
    int32_t niceValue;
    Hash<uint32_t, Path> blockedFiles;

    Path sandboxRoot;
    deserializer >> sandboxRoot;
    Sandbox::setRoot(sandboxRoot);
    deserializer >> id;
    deserializer >> socketFile;
    deserializer >> mProject;
    uint32_t count;
    deserializer >> count;
    mSources.resize(count);
    for (uint32_t i=0; i<count; ++i) {
        mSources[i].decode(deserializer, Source::IgnoreSandbox);
    }
    deserializer >> mSourceFile;
    deserializer >> indexerJobFlags;
    deserializer >> mVisitFileTimeout;
    deserializer >> mIndexDataMessageTimeout;
    deserializer >> connectTimeout;
    deserializer >> connectAttempts;
    deserializer >> niceValue;
    deserializer >> sServerOpts;
    deserializer >> mUnsavedFiles;
    deserializer >> mDataDir;
    deserializer >> mDebugLocations;
    deserializer >> blockedFiles;

    if (sServerOpts & Server::NoRealPath) {
        Path::setRealPathEnabled(false);
    }

#if 0
    while (true) {
        FILE *f = fopen((String("/tmp/stop_") + mSourceFile.fileName()).constData(), "r+");
        if (f) {
            fseek(f, 0, SEEK_END);
            fprintf(f, "Waiting ... %d\n", getpid());
            fclose(f);
            sleep(1);
        } else {
            break;
        }
    }
#endif

    const uint64_t parseTime = Rct::currentTimeMs();

    if (niceValue != INT_MIN) {
        errno = 0;
        if (nice(niceValue) == -1) {
            error() << "Failed to nice rp" << Rct::strerror();
        }
    }

    if (mSourceFile.isEmpty()) {
        error("No sourcefile");
        return false;
    }

    switch (mSources.size()) {
    case 0:
        error("No sourcefile");
        return false;
    case 1:
        if (!mSources.front().fileId) {
            error("Bad fileId");
            return false;
        }
        break;
    default:
        for (size_t i=1; i<mSources.size(); ++i) {
            if (!mSources.at(i).fileId || mSources.at(i).fileId != mSources.front().fileId) {
                error("Bad fileId");
                return false;
            }
        }
    }

    if (mProject.isEmpty()) {
        error("No project");
        return false;
    }

    if (ClangIndexer::state() == Stopped)
        return true;

    Location::init(blockedFiles);
    Location::set(mSourceFile, mSources.front().fileId);
    while (!mConnection->isConnected()) {
        if (mConnection->connectUnix(socketFile, connectTimeout))
            break;
        if (!--connectAttempts) {
            error("Failed to connect to rdm on %s (%dms timeout)", socketFile.constData(), connectTimeout);
            return false;
        }
        usleep(500 * 1000);
    }

    if (ClangIndexer::state() == Stopped)
        return true;
    // mLogFile = fopen(String::format("/tmp/%s", mSourceFile.fileName()).constData(), "w");
    mIndexDataMessage.setProject(mProject);
    mIndexDataMessage.setIndexerJobFlags(indexerJobFlags);
    mIndexDataMessage.setParseTime(parseTime);
    mIndexDataMessage.setId(id);

    assert(mConnection->isConnected());
    assert(mSources.front().fileId);
    mIndexDataMessage.files()[mSources.front().fileId] |= IndexDataMessage::Visited;
    bool ok = parse();
    if (ClangIndexer::state() == Stopped)
        return true;
    if (ok)
        ok = visit();
    if (ClangIndexer::state() == Stopped)
        return true;
    if (ok)
        ok = diagnose();

    String message = mSourceFile.toTilde();
    String err;

    StopWatch sw;
    int writeDuration = -1;
    bool hasUnit = false;
    for (const auto &u : mTranslationUnits) {
        if (u->unit) {
            hasUnit = true;
            break;
        }
    }
    if (!hasUnit || !writeFiles(RTags::encodeSourceFilePath(mDataDir, mProject, 0), err)) {
        message += " error";
        if (!err.isEmpty())
            message += (' ' + err);
    } else {
        writeDuration = sw.elapsed();
    }
    if (ClangIndexer::state() == Stopped)
        return true;
    message += String::format<16>(" in %lldms. ", mTimer.elapsed());
    if (mSources.size() > 1) {
        message += String::format("(%zu builds) ", mSources.size());
    }
    int cursorCount = 0;
    int symbolNameCount = 0;
    for (const auto &unit : mUnits) {
        cursorCount += unit.second->symbols.size();
        symbolNameCount += unit.second->symbolNames.size();
    }
    if (hasUnit) {
        String queryData;
        if (mFileIdsQueried)
            queryData = String::format(", %d queried %dms", mFileIdsQueried, mFileIdsQueriedTime);
        const char *format = "(%d syms, %d symNames, %d includes, %d of %d files, symbols: %d of %d, %d cursors, %zu bytes written%s%s) (%d/%d/%dms)";
        message += String::format<1024>(format, cursorCount, symbolNameCount,
                                        mIndexDataMessage.includes().size(), mIndexed,
                                        mIndexDataMessage.files().size(), mAllowed,
                                        mAllowed + mBlocked, mCursorsVisited,
                                        mIndexDataMessage.bytesWritten(),
                                        queryData.constData(), mIndexDataMessage.flags() & IndexDataMessage::UsedPCH ? ", pch" : "",
                                        mParseDuration, mVisitDuration, writeDuration);
    }
    bool paren = false;
    auto add = [&paren, &message](const char *string) {
        if (!paren) {
            message += " (";
            paren = true;
        } else {
            message += ",";
        }
        message += string;
    };
    if (mIndexDataMessage.indexerJobFlags() & IndexerJob::Dirty) {
        add("dirty");
    } else if (mIndexDataMessage.indexerJobFlags() & IndexerJob::Reindex) {
        add("reindex");
    }

    if (mFromCache) {
        add("cache");
    }
    if (mIndexDataMessage.indexerJobFlags() & IndexerJob::EditorActive) {
        add("active");
    } else if (mIndexDataMessage.indexerJobFlags() & IndexerJob::EditorOpen) {
        add("open");
    }

    if (paren)
        message += ")";

    mIndexDataMessage.setMessage(std::move(message));
    sw.restart();
    if (ClangIndexer::state() == Stopped)
        return true;
    debug() << "Sending index data message" << mIndexDataMessage.id();
    if (!mConnection->send(mIndexDataMessage)) {
        error() << "Couldn't send IndexDataMessage" << mSourceFile;
        return false;
    }
    mConnection->finished().connect(std::bind(&EventLoop::quit, EventLoop::eventLoop()));
    if (EventLoop::eventLoop()->exec(mIndexDataMessageTimeout) == EventLoop::Timeout) {
        error() << "Timed out sending IndexDataMessage" << mSourceFile;
        return false;
    }

    if (getenv("RDM_DEBUG_INDEXERMESSAGE"))
        error() << "Send took" << sw.elapsed() << "for" << mSourceFile;

    return true;
}

void ClangIndexer::onMessage(const std::shared_ptr<Message> &msg, const std::shared_ptr<Connection> &/*conn*/)
{
    assert(msg->messageId() == VisitFileResponseMessage::MessageId);
    const std::shared_ptr<VisitFileResponseMessage> vm = std::static_pointer_cast<VisitFileResponseMessage>(msg);
    mVisitFileResponseMessageVisit = vm->visit();
    mVisitFileResponseMessageFileId = vm->fileId();
    assert(EventLoop::eventLoop());
    EventLoop::eventLoop()->quit();
}

Location ClangIndexer::createLocation(const Path &sourceFile, unsigned int line, unsigned int col, bool *blockedPtr)
{
    uint32_t id = Location::fileId(sourceFile);
    Path resolved;
    if (!id) {
        bool ok;
        for (int i=0; i<4; ++i) {
            resolved = sourceFile.resolved(Path::RealPath, Path(), &ok);
            // if ok is false it means the file is gone, in case this happens
            // during a git pull or something we'll give it a couple of chances.
            if (ok)
                break;
            usleep(50000);
        }
        if (!ok)
            return Location();
        id = Location::fileId(resolved);
        if (id)
            Location::set(sourceFile, id);
    }
    assert(!resolved.contains("/../"));

    if (id) {
        if (blockedPtr) {
            Hash<uint32_t, Flags<IndexDataMessage::FileFlag> >::iterator it = mIndexDataMessage.files().find(id);
            if (it == mIndexDataMessage.files().end()) {
                // the only reason we already have an id for a file that isn't
                // in the mIndexDataMessage.mFiles is that it's blocked from the outset.
                // The assumption is that we never will go and fetch a file id
                // for a location without passing blockedPtr since any reference
                // to a symbol in another file should have been preceded by that
                // header in which case we would have to make a decision on
                // whether or not to index it. This is a little hairy but we
                // have to try to optimize this process.
#ifndef NDEBUG
                if (resolved.isEmpty())
                    resolved = sourceFile.resolved();
#endif
                assert(id);
                mIndexDataMessage.files()[id] = IndexDataMessage::NoFileFlag;
                *blockedPtr = true;
            } else if (!it->second) {
                *blockedPtr = true;
            }
        }
        return Location(id, line, col);
    }

    ++mFileIdsQueried;
    VisitFileMessage msg(resolved, mProject, mSources.front().fileId);

    mVisitFileResponseMessageFileId = UINT_MAX;
    mVisitFileResponseMessageVisit = false;
    mConnection->send(msg);
    StopWatch sw;
    EventLoop::eventLoop()->exec(mVisitFileTimeout);
    const int elapsed = sw.elapsed();
    mFileIdsQueriedTime += elapsed;
    switch (mVisitFileResponseMessageFileId) {
    case 0:
        return Location();
    case UINT_MAX:
        // timed out.
        if (mVisitFileResponseMessageFileId == UINT_MAX) {
            error() << "Error getting fileId for" << resolved << mLastCursor
                    << elapsed << mVisitFileTimeout;
        }
        exit(1);
    default:
        id = mVisitFileResponseMessageFileId;
        break;
    }
    assert(id);
    Flags<IndexDataMessage::FileFlag> &flags = mIndexDataMessage.files()[id];
    if (mVisitFileResponseMessageVisit) {
        flags |= IndexDataMessage::Visited;
        ++mIndexed;
    }
    // fprintf(mLogFile, "%s %s\n", file.second ? "WON" : "LOST", resolved.constData());

    Location::set(resolved, id);
    if (resolved != sourceFile)
        Location::set(sourceFile, id);

    if (blockedPtr)
        *blockedPtr = !mVisitFileResponseMessageVisit;
    return Location(id, line, col);
}

CXTranslationUnit ClangIndexer::unit(size_t u) const
{
    return mTranslationUnits[u]->unit;
}

static inline void tokenize(const char *buf, int start,
                            int *templateStart, int *templateEnd,
                            int *sectionCount, int sections[1024])
{
    int templateCount = 0;
    *templateStart = *templateEnd = -1;
    *sectionCount = 1;
    sections[0] = start;
    int functionStart = -1;
    int functionEnd = -1;

    int idx = start;
    while (true) {
        switch (buf[++idx]) {
        case '<':
            if (buf[idx + 1] == '<') {
                ++idx;
            } else if (functionStart == -1 && (idx - 8 < 0 || strncmp("operator", buf + idx - 8, 8) != 0)) {
                if (!templateCount++)
                    *templateStart = idx;
            }
            break;
        case '>':
            if (buf[idx + 1] == '>') {
                ++idx;
            } else if (functionStart == -1 && (idx - 8 < 0 || strncmp("operator", buf + idx - 8, 8) != 0)) {
                if (!--templateCount)
                    *templateEnd = idx;
            }
            break;
        case '(':
            if (!templateCount)
                functionStart = idx;
            break;
        case ')':
            if (!templateCount)
                functionEnd = idx;
            break;
        case ':':
            if (!templateCount && (functionStart == -1 || functionEnd != -1) && buf[idx + 1] == ':' && (*sectionCount) < 1024) {
                sections[(*sectionCount)++] = idx + 2;
                ++idx;
            }
            break;
        case '\0':
            if (templateCount) {
                *templateStart = *templateEnd = -1;
            }
            return;
        }
    }
}

String ClangIndexer::addNamePermutations(const CXCursor &cursor, Location location, RTags::CursorType cursorType)
{
    CXCursorKind kind = clang_getCursorKind(cursor);
    const CXCursorKind originalKind = kind;
    char buf[1024 * 512];
    int pos = sizeof(buf) - 1;
    buf[pos] = '\0';
    int cutoff = -1;

    CXCursor c = cursor;
    do {
        CXStringScope displayName(clang_getCursorDisplayName(c));
        const char *name = displayName.data();
        if (!name)
            break;
        const int len = strlen(name);
        if (!len)
            break;

        if (pos != sizeof(buf) - 1) {
            pos -= 2;
            if (pos >= 0)
                memset(buf + pos, ':', 2);
        }
        pos -= len;
        if (pos < 0) {
            error("SymbolName too long. Giving up");
            return String();
        }
        memcpy(buf + pos, name, len);

        c = clang_getCursorSemanticParent(c);
        kind = clang_getCursorKind(c);
        if (cutoff == -1) {
            switch (kind) {
            case CXCursor_ClassDecl:
            case CXCursor_ClassTemplate:
            case CXCursor_StructDecl:
                break;
            case CXCursor_Namespace:
                // namespaces can include all namespaces in their symbolname
                if (originalKind == CXCursor_Namespace)
                    break;
                RCT_FALL_THROUGH;
            default:
                cutoff = pos;
                break;
            }
        }
    } while (RTags::needsQualifiers(kind));

    if (static_cast<size_t>(pos) == sizeof(buf) - 1) {
        return String();
    }
    String type;
    String trailer;
    switch (originalKind) {
    case CXCursor_StructDecl:
        type = "struct ";
        break;
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
        type = "class ";
        break;
    case CXCursor_Namespace:
        type = "namespace ";
        break;
    case CXCursor_Destructor:
    case CXCursor_Constructor:
        break;
    default: {
        type = RTags::eatString(clang_getTypeSpelling(clang_getCanonicalType(clang_getCursorType(cursor))));
        if (originalKind == CXCursor_FunctionDecl || originalKind == CXCursor_CXXMethod || originalKind == CXCursor_FunctionTemplate) {
            const size_t idx = type.indexOf(" -> ");
            if (idx != String::npos)
                trailer = type.mid(idx);
        }
        const size_t paren = type.indexOf('(');
        if (paren != String::npos) {
            type.resize(paren);
        } else if (!type.isEmpty() && !type.endsWith('*') && !type.endsWith('&')) {
            type.append(' ');
        }
        break; }
    }

    if (cutoff == -1)
        cutoff = pos;

    String ret;
    if (!type.isEmpty()) {
        ret = type;
        ret.append(buf + cutoff, std::max<int>(0, sizeof(buf) - cutoff - 1));
        if (!trailer.isEmpty())
            ret += trailer;
        if (cursorType != RTags::Type_Reference) {
            unit(location.fileId())->symbolNames[ret].insert(location);
        }
    } else {
        ret.assign(buf + cutoff, std::max<int>(0, sizeof(buf) - cutoff - 1));
    }
    if (cursorType == RTags::Type_Reference) {
        return ret;
    }

    int templateStart, templateEnd, colonColonCount;
    int colonColons[1024];
    ::tokenize(buf, pos,
               &templateStart, &templateEnd,
               &colonColonCount, colonColons);
    assert((templateStart != -1) == (templateEnd != -1));

    // i == 0 --> with templates,
    // i == 1 without templates or without EnumConstantDecl part
    for (int i=0; i<2; ++i) {
        for (int j=0; j<colonColonCount; ++j) {
            const char *ch = buf + colonColons[j];
            String name(ch, std::max<int>(0, sizeof(buf) - (ch - buf) - 1));
            if (name.isEmpty())
                continue;
            unit(location.fileId())->symbolNames[name].insert(location);
            if (originalKind == CXCursor_ObjCClassMethodDecl) {
                const size_t idx = name.indexOf(':');
                if (idx != String::npos && idx > 0) {
                    name.resize(idx);
                    unit(location.fileId())->symbolNames[name].insert(location);
                }
            }
            if (!type.isEmpty() && (originalKind != CXCursor_ParmDecl || !strchr(ch, '('))) {
                // We only want to add the type to the final declaration for ParmDecls
                // e.g.
                // void foo(int)::bar
                // and
                // int bar
                //
                // not
                // int void foo(int)::bar
                // or
                // void foo(int)::int bar

                unit(location.fileId())->symbolNames[type + name].insert(location);
            }
        }

        if (i == 1 || (templateStart == -1 && originalKind != CXCursor_EnumConstantDecl)) {
            // nothing more to do
            break;
        }

        if (originalKind == CXCursor_EnumConstantDecl) { // remove CXCursor_EnumDecl
            // struct A { enum B { C } };
            // Will by default generate a A::B::C symbolname.
            // This code removes the B:: part from it
            if (colonColonCount > 2) {
                const char *last = buf + colonColons[colonColonCount - 1];
                const char *secondLast = buf + colonColons[colonColonCount - 2];
                const int len = (last - secondLast);
                memmove(buf + pos + len, buf + pos, secondLast - (buf + pos));
                pos += len;
            }
        } else { // remove templates
            assert(templateStart != -1);
            assert(templateEnd != -1);
            const int templateSize = (templateEnd - templateStart) + 1;
            memmove(buf + pos + templateSize, buf + pos, (buf + templateStart) - (buf + pos));
            pos += templateSize;
        }
        // ### We could/should just move the colon colon values but this
        // should be pretty quick and I don't want to write the code to
        // do it.
        ::tokenize(buf, pos,
                   &templateStart, &templateEnd,
                   &colonColonCount, colonColons);
    }

    return ret;
}

static inline CXCursor findDestructorForDelete(const CXCursor &deleteStatement)
{
    const CXCursor child = RTags::findFirstChild(deleteStatement);
    CXCursorKind kind = clang_getCursorKind(child);
    switch (kind) {
    case CXCursor_UnexposedExpr:
    case CXCursor_CallExpr:
        break;
    default:
        return clang_getNullCursor();
    }

    const CXCursor var = clang_getCursorReferenced(child);
    kind = clang_getCursorKind(var);
    switch (kind) {
    case CXCursor_ObjCIvarDecl:
    case CXCursor_VarDecl:
    case CXCursor_FieldDecl:
    case CXCursor_ParmDecl:
    case CXCursor_CXXMethod:
    case CXCursor_FunctionDecl:
    case CXCursor_ConversionFunction:
        break;
    default:
        if (!clang_isInvalid(kind)) {
            error() << "Got unexpected cursor" << deleteStatement << var;
            // assert(0);
        }
        return clang_getNullCursor();
    }

    CXCursor ref = RTags::findChild(var, CXCursor_TypeRef);
    if (ref != CXCursor_TypeRef)
        ref = RTags::findChild(var, CXCursor_TemplateRef);
    kind = clang_getCursorKind(ref);
    switch (kind) {
    case CXCursor_TypeRef:
    case CXCursor_TemplateRef:
        break;
    default:
        return clang_getNullCursor();
    }

    const CXCursor referenced = clang_getCursorReferenced(ref);
    kind = clang_getCursorKind(referenced);
    switch (kind) {
    case CXCursor_StructDecl:
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
        break;
    default:
        return clang_getNullCursor();
    }
    const CXCursor destructor = RTags::findChild(referenced, CXCursor_Destructor);
    return destructor;
}

CXChildVisitResult ClangIndexer::visitorHelper(CXCursor cursor, CXCursor, CXClientData data)
{
    if (ClangIndexer::state() == Stopped)
        return CXChildVisit_Break;
    ClangIndexer *indexer = static_cast<ClangIndexer*>(data);
    const CXChildVisitResult res = indexer->indexVisitor(cursor);
    if (res == CXChildVisit_Recurse)
        indexer->visit(cursor);
    return CXChildVisit_Continue;
}

CXChildVisitResult ClangIndexer::indexVisitor(CXCursor cursor)
{
    ++mCursorsVisited;
    // error() << "indexVisitor" << cursor;
    // FILE *f = fopen("/tmp/clangindex.log", "a");
    // String str;
    // Log(&str) << cursor;
    // fwrite(str.constData(), 1, str.size(), f);
    // fwrite("\n", 1, 1, f);
    // fclose(f);

    const CXCursorKind kind = clang_getCursorKind(cursor);
    const RTags::CursorType type = RTags::cursorType(kind);
    if (type == RTags::Type_Other) {
        return CXChildVisit_Recurse;
    }

    struct UpdateLastCursor {
        ~UpdateLastCursor() { func(); }
        std::function<void()> func;
    } call = { [this, cursor]() { mLastCursor = cursor; } };

    bool blocked = false;

    Location loc = createLocation(cursor, kind, &blocked);
    if (blocked) {
        ++mBlocked;
        return CXChildVisit_Continue;
    } else if (loc.isNull()) {
        // error() << "Got null" << cursor;
        return CXChildVisit_Recurse;
    }
    for (const String &debug : mDebugLocations) {
        if (debug == "all" || debug == loc) {
            Log log(LogLevel::Error);
            log << cursor;
            CXCursor ref = clang_getCursorReferenced(cursor);
            if (!clang_isInvalid(clang_getCursorKind(ref)) && ref != cursor) {
                log << "refs" << ref;
            }
            break;
        }
    }
    ++mAllowed;
    if (mLogFile) {
        String out;
        Log(&out) << cursor;
        fwrite(out.constData(), 1, out.size(), mLogFile);
        fwrite("\n", 1, 1, mLogFile);
    }

    if (testLog(LogLevel::VerboseDebug)) {
        Log log(LogLevel::VerboseDebug);
        log << cursor;
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (!clang_isInvalid(clang_getCursorKind(ref)) && ref != cursor) {
            log << "refs" << ref;
        }
    }

    if (Symbol::isClass(kind)) {
        mLastClass = loc;
    } else {
        if (kind == CXCursor_CXXBaseSpecifier) {
            handleBaseClassSpecifier(cursor);
            return CXChildVisit_Recurse;
        }
    }

    CXChildVisitResult visitResult = CXChildVisit_Recurse;
    switch (type) {
    case RTags::Type_Cursor:
        visitResult = handleCursor(cursor, kind, loc);
        break;
    case RTags::Type_Include:
        handleInclude(cursor, kind, loc);
        break;
    case RTags::Type_Literal:
        handleLiteral(cursor, kind, loc);
        break;
    case RTags::Type_Statement:
        visitResult = handleStatement(cursor, kind, loc);
        break;
    case RTags::Type_Reference:
        switch (kind) {
        case CXCursor_OverloadedDeclRef: {
            const int count = clang_getNumOverloadedDecls(cursor);
            for (int i=0; i<count; ++i) {
                const CXCursor ref = clang_getOverloadedDecl(cursor, i);
                handleReference(cursor, kind, loc, ref);
            }
            break; }
        case CXCursor_CXXDeleteExpr:
            handleReference(cursor, kind, loc, findDestructorForDelete(cursor));
            break;
        case CXCursor_CallExpr: {
            // uglehack, see rtags/tests/nestedClassConstructorCallUgleHack/
            List<Symbol::Argument> arguments;
            extractArguments(&arguments, cursor);
            Symbol *old = nullptr;
            Location oldLoc;
            std::swap(mLastCallExprSymbol, old);
            const CXCursor ref = clang_getCursorReferenced(cursor);
            bool handled = false;
            const CXCursorKind refKind = clang_getCursorKind(ref);
            if (refKind  == CXCursor_Constructor
                && (clang_getCursorKind(mLastCursor) == CXCursor_TypeRef || clang_getCursorKind(mLastCursor) == CXCursor_TemplateRef)) {
                handled = true;
                for (int pos = mParents.size() - 1; pos >= 0; --pos) {
                    const CXCursor &parent = mParents[pos];
                    const CXCursorKind k = clang_getCursorKind(parent);
                    if (k == CXCursor_VarDecl) {
                        handled = false;
                        break;
                    } else if (k == CXCursor_CallExpr) {
                        if (!clang_isInvalid(clang_getCursorKind(clang_getCursorReferenced(parent)))) {
                            handled = false;
                            break;
                        }
                    } else if (k != CXCursor_UnexposedExpr) {
                        break;
                    }
                }
                if (handled) {
                    loc = createLocation(mLastCursor);
                    handleReference(mLastCursor, kind, loc, ref);
                }
            } else if (refKind == CXCursor_FieldDecl) {
                handled = true;
            }
            if (!handled) {
                handleReference(cursor, kind, loc, ref);
            }
            List<Symbol::Argument> destArguments;
            extractArguments(&destArguments, ref);
            visit(cursor);
            if (mLastCallExprSymbol && !arguments.isEmpty()) {
                const Location invokedLocation = createLocation(ref, refKind);
                auto u = unit(loc);
                size_t idx = 0;
                for (const auto &arg : arguments) {
                    const auto destArg = destArguments.value(idx);
                    if (destArg.location.isNull())
                        break;
                    const Location start = arg.location;
                    Location end;
                    if (idx + 1 == arguments.size()) {
                        end = Location(start.fileId(), start.line(), start.column() + arg.length); // this falls apart with multi-line args
                    } else {
                        end = arguments.value(idx + 1).location;
                    }
                    auto it = u->symbols.lower_bound(start);
                    while (it != u->symbols.end() && it->first < end) {
                        auto &sym = it->second;
                        sym.argumentUsage.index = idx;
                        sym.argumentUsage.invokedFunction = invokedLocation;
                        sym.argumentUsage.argument = destArg;
                        sym.argumentUsage.invocation = mLastCallExprSymbol->location;
                        ++it;
                    }
                    ++idx;
                }

                mLastCallExprSymbol->arguments = std::move(arguments);
            }
            std::swap(old, mLastCallExprSymbol);
            visitResult = CXChildVisit_Continue;
            break; }
        default:
            handleReference(cursor, kind, loc, clang_getCursorReferenced(cursor));
            break;
        }
        break;
    case RTags::Type_Other:
        assert(0);
        break;
    }
    return visitResult;
}

static inline bool isImplicit(const CXCursor &cursor)
{
    return clang_equalLocations(clang_getCursorLocation(cursor),
                                clang_getCursorLocation(clang_getCursorSemanticParent(cursor)));
}

bool ClangIndexer::superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                            Location location, const CXCursor &/*ref*/,
                                                            Symbol **cursorPtr)
{
    // This is for references to superclass template functions. Awful awful
    // shit. See https://github.com/Andersbakken/rtags/issues/62 and commit
    // for details. I really should report this as a bug.
    if (cursorPtr)
        *cursorPtr = nullptr;
    if (kind != CXCursor_MemberRefExpr && clang_getCursorKind(mParents.last()) != CXCursor_CallExpr)
        return false;

    const CXCursor templateRef = RTags::findChild(cursor, CXCursor_TemplateRef);
    if (templateRef != CXCursor_TemplateRef)
        return false;

    const CXCursor classTemplate = clang_getCursorReferenced(templateRef);
    if (classTemplate != CXCursor_ClassTemplate)
        return false;
    FILE *f = fopen(location.path().constData(), "r");
    if (!f)
        return false;

    const CXSourceRange range = clang_getCursorExtent(cursor);
    const CXSourceLocation end = clang_getRangeEnd(range);
    unsigned int offset;
    clang_getSpellingLocation(end, nullptr, nullptr, nullptr, &offset);

    String name;
    while (offset > 0) {
        fseek(f, --offset, SEEK_SET);
        char ch = static_cast<char>(fgetc(f));
        if (isalnum(ch) || ch == '_' || ch == '~') {
            name.prepend(ch);
        } else {
            break;
        }
    }
    fclose(f);
    if (!name.isEmpty()) {
        RTags::Filter out;
        out.kinds.insert(CXCursor_MemberRefExpr);
        const int argCount = RTags::children(mParents.last(), RTags::Filter(), out).size();
        RTags::Filter in(RTags::Filter::And);
        in.names.insert(name);
        in.argumentCount = argCount;
        const List<CXCursor> alternatives = RTags::children(classTemplate, in);
        switch (alternatives.size()) {
        case 1:
            // ### not sure this is correct with line/col
            return handleReference(cursor, kind,
                                   Location(location.fileId(), location.line(), location.column() + 1),
                                   alternatives.first(), cursorPtr);
            break;
        case 0:
            break;
        default:
            warning() << "Can't decide which of these symbols are right for me"
                      << cursor << alternatives
                      << "Need to parse types";
            break;
        }
    }
    return false;
}

bool ClangIndexer::handleReference(const CXCursor &cursor, CXCursorKind kind, Location location, CXCursor ref, Symbol **cursorPtr)
{
    if (cursorPtr)
        *cursorPtr = nullptr;
    // error() << "handleReference" << cursor << kind << location << ref;
    const CXCursorKind refKind = clang_getCursorKind(ref);
    if (clang_isInvalid(refKind)) {
        return superclassTemplateMemberFunctionUgleHack(cursor, kind, location, ref, cursorPtr);
    }

    const CXCursor originalRef = ref;
    bool isOperator = false;
    if (kind == CXCursor_CallExpr && (refKind == CXCursor_CXXMethod
                                      || refKind == CXCursor_FunctionDecl
                                      || refKind == CXCursor_FunctionTemplate)) {
        // These are bullshit. for this construct:
        // foo.bar();
        // the position of the cursor is at the foo, not the bar.
        // They are not interesting for followLocation, renameSymbol or find
        // references so we toss them.
        // For functions it can be the position of the namespace.
        // E.g. Foo::bar(); cursor is on Foo
        // For constructors they happen to be the only thing we have that
        // actually refs the constructor and not the class so we have to keep
        // them for that.
        return false;
    }

    Location refLoc = createLocation(ref, refKind);
    if (!refLoc.isValid()) {
        // ### THIS IS NOT SOLVED
        // if (kind == CXCursor_ObjCMessageExpr) {
        //    mIndexDataMessage.mPendingReferenceMap[RTags::eatString(clang_getCursorUSR(clang_getCanonicalCursor(ref)))].insert(location);
        //     // insert it, we'll hook up the target and references later
        //     return handleCursor(cursor, kind, location, cursorPtr);
        // }
        return false;
    }

    switch (refKind) {
    case CXCursor_Constructor:
    case CXCursor_CXXMethod:
    case CXCursor_FunctionDecl:
    case CXCursor_Destructor:
    case CXCursor_FunctionTemplate: {
        bool visitReference = false;
        ref = resolveTemplate(ref, refLoc, &visitReference);
        if (visitReference && (kind == CXCursor_DeclRefExpr || kind == CXCursor_MemberRefExpr)) {
            mTemplateSpecializations.insert(originalRef);
        }

        if (refKind == CXCursor_FunctionDecl)
            break;
        if (refKind == CXCursor_Constructor || refKind == CXCursor_Destructor) {
            if (isImplicit(ref)) {
                return false;
            }
        } else {
            CXStringScope scope = clang_getCursorDisplayName(ref);
            const char *data = scope.data();
            if (data) {
                const int len = strlen(data);
                if (len > 8 && !strncmp(data, "operator", 8) && !isalnum(data[8]) && data[8] != '_') {
                    if (isImplicit(ref)) {
                        return false; // eat implicit operator calls
                    }
                    isOperator = true;
                }
            }
        }
        break; }
    default:
        ref = resolveTemplateUsr(ref);
        break;
    }

    const String refUsr = RTags::usr(ref);
    if (refUsr.isEmpty()) {
        return false;
    }

    FindResult result;
    auto reffedCursor = findSymbol(refLoc, &result);
    Map<String, uint16_t> &targets = unit(location)->targets[location];
    if (result == NotFound && !mUnionRecursion) {
        CXCursor parent = clang_getCursorSemanticParent(ref);
        CXCursor best = clang_getNullCursor();
        while (true) {
            if (clang_getCursorKind(parent) != CXCursor_UnionDecl)
                break;
            best = parent;
            parent = clang_getCursorSemanticParent(parent);
        }
        if (best == CXCursor_UnionDecl) {
            mUnionRecursion = true;
            // for anonymous unions we don't get to their fields with normal
            // recursing of the AST. In these cases we visit the union decl
            visit(best);
            mUnionRecursion = false;
            reffedCursor = findSymbol(refLoc, &result);
        }
    }
    const int16_t refTargetValue = RTags::createTargetsValue(refKind, clang_isCursorDefinition(ref));

    Symbol *c = &unit(location)->symbols[location];
    assert(c);
    bool setTarget = true;
    if (c->kind == CXCursor_MacroExpansion) {
        for (const auto &t : targets) {
            if (RTags::targetsValueKind(t.second) == CXCursor_MacroDefinition) {
                for (const auto &u : mUnits) { // ### should only search the ones we depend on
                    const auto it = u.second->usrs.find(t.first);
                    if (it != u.second->usrs.end()) {
                        auto mit = mMacroTokens.find(*it->second.begin());
                        if (mit != mMacroTokens.end()) {
                            const String id = RTags::eatString(clang_getCursorSpelling(cursor));
                            auto idit = mit->second.data.find(id);
                            if (idit != mit->second.data.end()) {
                                List<Location> &locs = idit->second.locations;
                                assert(!locs.isEmpty());
                                location = locs.front();
                                if (locs.size() == 1) {
                                    if (mit->second.data.size() == 1) {
                                        mMacroTokens.erase(mit);
                                    } else {
                                        mit->second.data.erase(idit);
                                    }
                                } else {
                                    locs.remove(0, 1);
                                }
                                std::shared_ptr<Unit> uu = unit(location);
                                c = &uu->symbols[location];
                                Map<String, uint16_t> &tt = uu->targets[location];
                                tt[refUsr] = refTargetValue;
                                setTarget = false;
                            }
                        }
                        break;
                    }
                }
                break;
            }
        }
    }

    assert(!refUsr.isEmpty());
    targets[refUsr] = refTargetValue;

    if (mInTemplateFunction)
        c->flags |= Symbol::TemplateReference;

    if (cursorPtr)
        *cursorPtr = c;

    // We need the new cursor to replace the symbolLength. This is important
    // in the following case:
    // struct R { R(const &r); ... }
    // R foo();
    // ...
    // R r = foo();

    // The first cursor on foo() will be a reference to the copy constructor and
    // this cursor will have a symbolLength of 1. Thus you won't be able to jump
    // to foo from the o. This is fixed by making sure the newer target, if
    // better, gets to decide on the symbolLength

    // The !isCursor is var decls and field decls where we set up a target even
    // if they're not considered references

    if (setTarget && !c->isNull()) {
        if (RTags::isCursor(c->kind))
            return true;
        auto best = targets.end();
        int bestRank = RTags::targetRank(RTags::targetsValueKind(refTargetValue));
        for (auto it = targets.begin(); it != targets.end(); ++it) {
            const int r = RTags::targetRank(RTags::targetsValueKind(it->second));
            if (r > bestRank || (r == bestRank && RTags::targetsValueIsDefinition(it->second))) {
                bestRank = r;
                best = it;
            }
        }
        if (best != targets.end() && best->first != refUsr) { // another target is better
            return true;
        }
    }

#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 35)
    if (result == Found) {
        c->size = reffedCursor.size;
        c->alignment = reffedCursor.alignment;
    } else {
        const CXType type = clang_getCursorType(ref);
        if (type.kind != CXType_LValueReference
            && type.kind != CXType_RValueReference
            && type.kind != CXType_Auto
            && type.kind != CXType_Unexposed) {
            c->size = std::max<uint16_t>(0, clang_Type_getSizeOf(type));
            c->alignment = std::max<int16_t>(-1, clang_Type_getAlignOf(type));
        }
    }
#endif

    CXSourceRange range = clang_getCursorExtent(cursor);
    uint16_t symLength;
    setRange(*c, range, &symLength);
    c->kind = kind;
    c->location = location;

    c->symbolName = RTags::eatString(clang_getCursorSpelling(cursor));
    if (c->symbolName.isEmpty())
        c->symbolName = (result == Found ? reffedCursor.symbolName : addNamePermutations(ref, refLoc, RTags::Type_Reference));
    if (isOperator) {
        c->symbolLength = symLength;
    } else {
        c->symbolLength = result == Found ? reffedCursor.symbolLength : symbolLength(refKind, ref);
    }
    if (!c->symbolLength) {
        unit(location)->symbols.remove(location);
        if (cursorPtr)
            *cursorPtr = nullptr;
        return false;
    }
    setType(*c, clang_getCursorType(kind == CXCursor_MemberRefExpr ? ref : cursor));
    if (RTags::isFunction(refKind)) {
        mLastCallExprSymbol = c;
    }

    if (mInTemplateFunction && !mParents.isEmpty()) {
        if ((kind == CXCursor_DeclRefExpr && mParents.last() == CXCursor_MemberRefExpr)
            || (kind == CXCursor_TypeRef && mParents.last() == CXCursor_DeclRefExpr)) {
            CXSourceRange parentRange = clang_getCursorExtent(mParents.last());
            CXToken *tokens = nullptr;
            unsigned numTokens = 0;
            auto tu = mTranslationUnits.at(mCurrentTranslationUnit)->unit;
            clang_tokenize(tu, parentRange, &tokens, &numTokens);
            for (size_t i=0; i<numTokens; ++i) {
                const CXTokenKind k = clang_getTokenKind(tokens[i]);
                if (k == CXToken_Punctuation) {
                    const CXStringScope str(clang_getTokenSpelling(tu, tokens[i]));
                    if ((str == "->" || str == "." || str == "::") && ++i < numTokens) {
                        assert(i < numTokens);
                        CXSourceRange memberRange = clang_getTokenExtent(tu, tokens[i]);
                        unsigned line, column;
                        clang_getSpellingLocation(clang_getRangeStart(memberRange), nullptr, &line, &column, nullptr);
                        const CXStringScope memberSpelling(clang_getTokenSpelling(tu, tokens[i]));
                        const Location loc(location.fileId(), line, column);
                        Symbol &sym = unit(loc)->symbols[loc];
                        sym.location = loc;
                        sym.symbolName = memberSpelling.data();
                        sym.symbolLength = sym.symbolName.size();
                        if (kind == CXCursor_DeclRefExpr) {
                            sym.kind = CXCursor_MemberRefExpr;
                        } else {
                            sym.kind = CXCursor_DeclRefExpr; // yes this is weird
                        }
                        sym.flags = Symbol::TemplateReference;
                        setType(sym, clang_getCursorType(mParents.last()));
                        setRange(sym, memberRange);
                        if (kind == CXCursor_DeclRefExpr) // there might be more than one level of ::
                            break;
                    }
                }
            }

            clang_disposeTokens(tu, tokens, numTokens);
        }
    }

    if (refKind == CXCursor_FunctionDecl && c->symbolName == "make_shared") {
        handleMakeShared(cursor, targets);
    }

    return true;
}


static inline bool hasDefaultArg(const CXCursor &cursor)
{
    static RTags::Filter filterOut;
    static bool first = true;
    if (first) {
        first = false;
        filterOut.kinds.insert(CXCursor_TypeRef);
    }
    return RTags::children(cursor, RTags::Filter(), filterOut).size();
}

enum MatchTypeResult {
    Mismatch,
    Cast,
    Match
};

static inline bool compareTypeString(const CXType &argument, const CXType &candidate)
{
    String aa = RTags::eatString(clang_getTypeSpelling(argument));
    while (aa.size() > 0 && (aa[aa.size() - 1] == '&' || aa[aa.size() - 1] == ' '))
        aa.chop(1);
    String bb = RTags::eatString(clang_getTypeSpelling(candidate));
    while (bb.size() > 0 && (bb[bb.size() - 1] == '&' || bb[bb.size() - 1] == ' '))
        bb.chop(1);
    if (bb.startsWith("const ") && !aa.startsWith("const ")) {
        bb.remove(0, 6);
    }

    return aa == bb;
}

static MatchTypeResult matchTypes(CXCursor argument, const CXCursor &candidate)
{
    CXType atype;
    if (clang_getCursorKind(argument) == CXCursor_CallExpr) {
        atype = clang_getCanonicalType(clang_getCursorResultType(clang_getCursorReferenced(argument)));
    } else {
        atype = clang_getCanonicalType(clang_getCursorType(argument));
    }
    const CXType ctype = clang_getCanonicalType(clang_getCursorType(candidate));
    if (ctype.kind == CXType_Pointer) {
        if (atype.kind == CXType_NullPtr)
            return Match;
        if (ctype.kind != CXType_Pointer)
            return Mismatch;
        if (compareTypeString(atype, ctype))
            return Match;
        return Cast;
    }

    if (atype.kind == CXType_Record) {
        if (ctype.kind != CXType_Record && ctype.kind != CXType_LValueReference)
            return Mismatch;
        // gotta check const
        if (compareTypeString(atype, ctype))
            return Match;
        return Cast;
    }

    if (ctype.kind == CXType_LValueReference || ctype.kind == CXType_RValueReference) {
        if (atype.kind != ctype.kind)
            return Mismatch;
        if (compareTypeString(clang_getPointeeType(atype), clang_getPointeeType(ctype)))
            return Match;
        return Mismatch;
    }

    if (atype.kind == ctype.kind) {
        return Match;
    }

    if (RTags::isNumber(atype.kind) && RTags::isNumber(ctype.kind)) {
        return Cast;
    }

    const CXStringScope ascope = clang_getTypeSpelling(atype);
    const CXStringScope cscope = clang_getTypeSpelling(ctype);
    if (!strcmp(clang_getCString(ascope), clang_getCString(cscope))) {
        return Match;
    }

    // error() << "compared strings" << clang_getCString(ascope) << clang_getCString(cscope);
    // error() << "a" << atype << "c" << ctype;

    // for (const auto &aa : RTags::children(argument)) {
    //     error() << "aa" << aa;
    // }

    // for (const auto &cc : RTags::children(candidate)) {
    //     error() << "cc" << cc;
    // }

    // auto cc = RTags::children(candidate);
    return Mismatch;
}

void ClangIndexer::handleMakeShared(const CXCursor &cursor, Map<String, uint16_t> &targets)
{
    CXCursor ref = clang_getCursorReferenced(cursor);
    CXCursor p1 = clang_getCursorSemanticParent(ref);
    CXCursor p2 = clang_getCursorSemanticParent(p1);
    if (clang_getCursorKind(p1) != CXCursor_Namespace)
        return;
    switch (clang_getCursorKind(p2)) {
    case CXCursor_TranslationUnit:
        if (RTags::eatString(clang_getCursorSpelling(p1)) != "std")
            return;
        break;
    case CXCursor_Namespace:
        if (RTags::eatString(clang_getCursorSpelling(p2)) != "std")
            return;
        break;
    default:
        return;
    }

    CXCursor call = clang_getNullCursor();
    for (int i=mParents.size() - 1; i>=0; --i) {
        if (clang_getCursorKind(mParents[i]) == CXCursor_CallExpr) {
            call = mParents[i];
            break;
        } else if (!i) {
            error() << "DIDN'T FIND CALL" << cursor;
            return;
        }
    }

    CXType clazzType = clang_Cursor_getTemplateArgumentType(ref, 0);
    CXCursor clazz = clang_getTypeDeclaration(clazzType);
    const int refCount = clang_Cursor_getNumArguments(call);

    static RTags::Filter filter;
    static bool first = true;
    if (first) {
        filter.kinds.insert(CXCursor_Constructor);
        first = false;
    }

    List<CXCursor> constructors = RTags::children(clazz, filter);
    List<String> usrs;
    usrs.reserve(constructors.size());
    size_t i=0;
    while (i<constructors.size()) {
        const CXCursor &cc = constructors[i];
        String usr = RTags::usr(cc);
        if (usr.isEmpty()) {
            constructors.removeAt(i);
            continue;
        }

        int count = clang_Cursor_getNumArguments(cc);
        if (count < refCount) {
            constructors.removeAt(i);
            continue;
        }

        if (count > 0) {
            for (int ii=0; ii<count; ++ii) {
                const CXCursor arg = clang_Cursor_getArgument(cc, ii);
                if (ii == refCount) {
                    if (!hasDefaultArg(arg)) {
                        count = -1;
                    }
                    break;
                }
            }
            if (count == -1) {
                constructors.removeAt(i);
                continue;
            }
        }
        usrs.append(std::move(usr));
        ++i;
    }

    assert(constructors.size() == usrs.size());
    if (constructors.size() == 1) {
        const int16_t refTargetValue = RTags::createTargetsValue(CXCursor_Constructor, clang_isCursorDefinition(constructors[0]));
        targets[usrs[0]] = refTargetValue;
    } else if (!constructors.isEmpty()) {
        List<std::pair<size_t, MatchTypeResult> > matched;
        matched.reserve(constructors.size());
        bool hasMatch = false;
        for (size_t ci=0; ci<constructors.size(); ++ci) {
            const CXCursor &cc = constructors[ci];
            MatchTypeResult res = Match;
            for (int ii=0; ii<refCount; ++ii) {
                const MatchTypeResult m = matchTypes(clang_Cursor_getArgument(call, ii), clang_Cursor_getArgument(cc, ii));
                if (m == Mismatch) {
                    res = m;
                    break;
                } else if (m == Cast) {
                    res = Cast;
                }
            }
            if (res != Mismatch) {
                matched.append(std::make_pair(ci, res));
                if (res == Match)
                    hasMatch = true;
            }
        }
        if (matched.size() == 0) {
            for (size_t matchIdx = 0; matchIdx<constructors.size(); ++matchIdx) {
                const int16_t refTargetValue = RTags::createTargetsValue(CXCursor_Constructor, clang_isCursorDefinition(constructors[matchIdx]));
                targets[usrs[matchIdx]] = refTargetValue;
            }
        } else {
            for (std::pair<size_t, MatchTypeResult> match : matched) {
                if (hasMatch && match.second == Cast)
                    continue;
                const int16_t refTargetValue = RTags::createTargetsValue(CXCursor_Constructor, clang_isCursorDefinition(constructors[match.first]));
                targets[usrs[match.first]] = refTargetValue;
            }
        }
    }
}

std::unordered_set<CXCursor> ClangIndexer::addOverriddenCursors(const CXCursor &c, Location location)
{
    // error() << "addOverriddenCursors" << cursor << location;
    std::unordered_set<CXCursor> ret;
    std::function<void(const CXCursor &)> process = [location, &ret, this, &process](const CXCursor &cursor) {
        CXCursor *overridden;
        unsigned int count;
        clang_getOverriddenCursors(cursor, &overridden, &count);
        if (overridden) {
            ret.insert(cursor);
            for (unsigned int i=0; i<count; ++i) {
                // error() << location << "got" << i << count << loc;

                const CXCursor resolved = resolveTemplate(overridden[i]);
                ret.insert(resolved);
                const String usr = RTags::usr(resolved);
                assert(!usr.isEmpty());
                // assert(!locCursor.usr.isEmpty());

                // error() << location << "targets" << overridden[i];
                unit(location)->targets[location][usr] = 0;
                process(overridden[i]);
            }
            clang_disposeOverriddenCursors(overridden);
        }
    };
    process(c);
    return ret;
}

void ClangIndexer::handleInclude(const CXCursor &cursor, CXCursorKind kind, Location location)
{
    assert(kind == CXCursor_InclusionDirective);
    (void)kind;
    CXFile includedFile = clang_getIncludedFile(cursor);
    if (includedFile) {
        const Location refLoc = createLocation(includedFile, 1, 1);
        if (!refLoc.isNull()) {
            Symbol &c = unit(location)->symbols[location];
            if (!c.isNull())
                return;

            String include = "#include ";
            Path path = refLoc.path();
            assert(mSources.front().fileId);
            unit(location)->symbolNames[(include + path)].insert(location);
            unit(location)->symbolNames[(include + path.fileName())].insert(location);
            mIndexDataMessage.includes().push_back(std::make_pair(location.fileId(), refLoc.fileId()));
            c.symbolName = "#include " + RTags::eatString(clang_getCursorDisplayName(cursor));
            c.kind = cursor.kind;
            c.symbolLength = c.symbolName.size() + 2;
            c.location = location;
            unit(location)->targets[location][refLoc.toString(Location::NoColor|Location::ConvertToRelative)] = 0; // ### what targets value to create for this?
            // this fails for things like:
            // # include    <foobar.h>
            return;
        }
    }

    error() << "handleInclude failed" << includedFile << cursor;
}

void ClangIndexer::handleLiteral(const CXCursor &cursor, CXCursorKind kind, Location location)
{
    auto tu = mTranslationUnits.at(mCurrentTranslationUnit)->unit;
    auto u = unit(location);
    // error() << location << kind
    //         << RTags::eatString(clang_getCursorDisplayName(cursor))
    //         << RTags::eatString(clang_getCursorSpelling(cursor))
    //         << clang_getCursorType(cursor).kind;

    CXType type = clang_getCursorType(cursor);
    // error() << location << kind << displayName;
    Symbol &s = u->symbols[location];
    if (!s.isNull())
        return;
    s.location = location;
    s.kind = kind;
    setType(s, type);
    CXSourceRange range = clang_getCursorExtent(cursor);
    setRange(s, range, &s.symbolLength);

    String symbolName;
    if (kind != CXCursor_StringLiteral) {
        CXToken *tokens = nullptr;
        unsigned numTokens = 0;
        clang_tokenize(tu, range, &tokens, &numTokens);
        if (numTokens) {
            symbolName = RTags::eatString(clang_getTokenSpelling(tu, tokens[0]));
        } else {
            Log(&symbolName) << type.kind;
        }

        clang_disposeTokens(tu, tokens, numTokens);
    } else {
        symbolName = RTags::eatString(clang_getCursorSpelling(cursor));
    }
    s.symbolName = symbolName;
    u->symbolNames[symbolName].insert(location);
    s.symbolLength = symbolName.size();
}

CXChildVisitResult ClangIndexer::handleStatement(const CXCursor &cursor, CXCursorKind kind, Location location)
{
    auto u = unit(location);
    // error() << "got dude" << kind << location;
    switch (kind) {
    case CXCursor_CompoundStmt: {
        Symbol &c = u->symbols[location];
        if (!c.isNull()) {
            break;
        }
        setRange(c, clang_getCursorExtent(cursor));
        const Scope scope = {
            Scope::Other, nullptr,
            Location(location.fileId(), c.startLine, c.startColumn),
            Location(location.fileId(), c.endLine, c.endColumn - 1)
        };
        if (mScopeStack.isEmpty() || mScopeStack.back().end != scope.end) {
            c.location = location;
            c.kind = kind;
            c.symbolName = "{}";
            c.symbolLength = 1;
            // should it have a symbolLength?
            mScopeStack.append(scope);
            visit(cursor);
            mScopeStack.removeLast();
        } else {
            // this is the function body, no need for this CompoundStmt
            u->symbols.remove(location);
            visit(cursor);
        }
        return CXChildVisit_Continue; }
    case CXCursor_ReturnStmt: {
        Symbol &c = u->symbols[location];
        if (!c.isNull())
            break;

        for (int i=mScopeStack.size() - 1; i>=0; --i) {
            const auto &scope = mScopeStack.at(i);
            if (scope.type == Scope::FunctionDefinition) {
                c.kind = kind;
                c.symbolName = "return";
                u->symbolNames[c.symbolName].insert(location);
                c.kind = kind;
                c.symbolLength = 6;
                c.location = location;
                setRange(c, clang_getCursorExtent(cursor));
                u->targets[location][scope.start.toString(Location::NoColor|Location::ConvertToRelative)] = 0;
                break;
            }
        }
        if (!c.symbolLength)
            u->symbols.remove(location);
        break; }
    case CXCursor_ForStmt:
    case CXCursor_WhileStmt:
    case CXCursor_DoStmt:
    case CXCursor_IfStmt:
    case CXCursor_SwitchStmt: {
        Symbol &c = u->symbols[location];
        if (!c.isNull())
            break;
        setRange(c, clang_getCursorExtent(cursor));
        c.kind = kind;
        switch (kind) {
        case CXCursor_SwitchStmt: c.symbolName = "switch"; break;
        case CXCursor_IfStmt: c.symbolName = "if"; break;
        case CXCursor_ForStmt: c.symbolName = "for"; break;
        case CXCursor_WhileStmt: c.symbolName = "while"; break;
        case CXCursor_DoStmt: c.symbolName = "do"; break;
        default: assert(0); break;
        }
        u->symbolNames[c.symbolName].insert(location);
        c.symbolLength = c.symbolName.size();
        c.location = location;
        if (kind != CXCursor_IfStmt) {
            const Loop loop = {
                kind,
                Location(location.fileId(), c.startLine, c.startColumn),
                Location(location.fileId(), c.endLine, c.endColumn)
            };

            mLoopStack.append(loop);
            visit(cursor);
            mLoopStack.removeLast();
            return CXChildVisit_Continue;
        }
        break; }
    case CXCursor_ContinueStmt:
    case CXCursor_BreakStmt: {
        Symbol &c = u->symbols[location];
        if (!c.isNull())
            break;
        Location target;
        for (int i = mLoopStack.size() - 1; i>=0; --i) {
            const auto &loop = mLoopStack.at(i);
            if (kind == CXCursor_BreakStmt) {
                target = loop.end;
                break;
            } else if (loop.kind != CXCursor_SwitchStmt) {
                target = loop.start;
                break;
            }
        }
        if (target.isNull()) {
            u->symbols.remove(location);
            break;
        }
        setRange(c, clang_getCursorExtent(cursor));
        c.symbolName = kind == CXCursor_BreakStmt ? "break" : "continue";
        u->symbolNames[c.symbolName].insert(location);
        c.kind = kind;
        c.symbolLength = c.symbolName.size();
        c.location = location;
        u->targets[location][target.toString(Location::NoColor|Location::ConvertToRelative)] = 0;
        break; }
    default:
        break;
    }
    return CXChildVisit_Recurse;
}

void ClangIndexer::handleBaseClassSpecifier(const CXCursor &cursor)
{
    auto &lastClass = unit(mLastClass)->symbols[mLastClass];
    if (!lastClass.isClass()) {
        // this happens with some weird macros in /usr/include/dispatch/io.h:161:1 and others on Mac
        // error() << "Couldn't find class for" << cursor << mLastClass;
        return;
    }
    CXCursor ref = clang_getCursorReferenced(cursor);
    if (clang_isInvalid(clang_getCursorKind(ref))) { // this happens when the base class is a template parameter
        return;
    }

    while (true) {
        CXCursor tmp = resolveTypedef(resolveTemplate(ref));
        if (tmp == ref) {
            break;
        } else {
            ref = std::move(tmp);
        }
    }
    const String usr = RTags::usr(ref);
    if (usr.isEmpty()) {
        warning() << "Couldn't find usr for" << clang_getCursorReferenced(cursor) << cursor << mLastClass;
        return;
    }
    assert(!usr.isEmpty());
    lastClass.baseClasses << usr;
}

void ClangIndexer::extractArguments(List<Symbol::Argument> *arguments, const CXCursor &cursor)
{
    assert(arguments);
    List<CXCursor> args;
    const int count = std::max(0, RTags::cursorArguments(cursor, &args));
    arguments->resize(count);
    for (int i=0; i<count; ++i) {
        auto &ref = (*arguments)[i];
        CXCursor arg = args[i];
        CXSourceRange range = clang_getCursorExtent(arg);
        unsigned startOffset, endOffset;

        ref.location = createLocation(clang_getRangeStart(range), nullptr, &startOffset);
        clang_getSpellingLocation(clang_getRangeEnd(range), nullptr, nullptr, nullptr, &endOffset);
        ref.length = endOffset - startOffset;
        ref.cursor = createLocation(arg);
    }
}

CXChildVisitResult ClangIndexer::handleCursor(const CXCursor &cursor, CXCursorKind kind,
                                              Location location, Symbol **cursorPtr)
{
    auto tu = mTranslationUnits.at(mCurrentTranslationUnit)->unit;
    const String usr = RTags::usr(cursor);
    // error() << "Got a cursor" << cursor;
    Symbol &c = unit(location)->symbols[location];
    if (cursorPtr)
        *cursorPtr = &c;
    if (!c.isNull()) {
        if (c.kind == CXCursor_MacroExpansion) {
            addNamePermutations(cursor, location, RTags::Type_Cursor);
            unit(location)->usrs[usr].insert(location);
        }
        return CXChildVisit_Recurse;
    }

    // if (mLogFile) {
    //     String out;
    //     Log(&out) << cursor << a;
    //     fwrite(out.constData(), 1, out.size(), mLogFile);
    //     fwrite("\n", 1, 1, mLogFile);
    // }
    CXStringScope name = clang_getCursorSpelling(cursor);
    const char *cstr = name.data();
    c.symbolLength = cstr ? strlen(cstr) : 0;
    const CXType type = clang_getCursorType(cursor);
    setType(c, type);
    c.location = location;
    c.usr = usr;
    if (!c.symbolLength) {
        // this is for these constructs:
        // typedef struct {
        //    int a;
        // } foobar;
        //
        // We end up not getting a spelling for the cursor

        switch (kind) {
        case CXCursor_ClassDecl:
            c.symbolLength = 5;
            c.symbolName = "class";
            break;
        case CXCursor_UnionDecl:
            c.symbolLength = 5;
            c.symbolName = "union";
            break;
        case CXCursor_StructDecl:
            c.symbolLength = 6;
            c.symbolName = "struct";
            break;
        case CXCursor_LambdaExpr:
            c.symbolLength = 2;
            c.symbolName = c.typeName;
            break;
        default:
            unit(location)->symbols.remove(location);
            if (cursorPtr)
                *cursorPtr = nullptr;
            return CXChildVisit_Recurse;
        }
    } else {
        if (kind == CXCursor_VarDecl || kind == CXCursor_ParmDecl) {
            RTags::Auto resolvedAuto;
            if (RTags::resolveAuto(cursor, &resolvedAuto)) {
                c.flags |= Symbol::Auto;
                if (resolvedAuto.type.kind != CXType_Invalid) {
                    setType(c, resolvedAuto.type);
                } else {
                    warning() << "Couldn't resolve auto for" << cursor;
                }
            }
        }

        c.symbolName = addNamePermutations(cursor, location, RTags::Type_Cursor);
    }

    if (clang_isCursorDefinition(cursor))
        c.flags |= Symbol::Definition;

    const CXSourceRange range = clang_getCursorExtent(cursor);
    setRange(c, range);

    switch (kind) {
    case CXCursor_ParmDecl:
    case CXCursor_VarDecl: {
        if (type.kind != CXType_Record || mScopeStack.isEmpty() || mScopeStack.back().type == Scope::FunctionDeclaration)
            break;

        CXCursor ref = RTags::findChild(cursor, CXCursor_TypeRef);
        if (ref != CXCursor_TypeRef)
            ref = RTags::findChild(cursor, CXCursor_TemplateRef);
        switch (clang_getCursorKind(ref)) {
        case CXCursor_TypeRef:
        case CXCursor_TemplateRef: {
            const CXCursor referenced = clang_getCursorReferenced(ref);
            switch (clang_getCursorKind(referenced)) {
            case CXCursor_StructDecl:
            case CXCursor_ClassDecl:
            case CXCursor_ClassTemplate: {
                const CXCursor destructor = RTags::findChild(referenced, CXCursor_Destructor);
                if (RTags::isValid(destructor)) {
                    const String destructorUsr = RTags::usr(destructor);
                    assert(!destructorUsr.isEmpty());
                    const Location scopeEndLocation = mScopeStack.back().end;
                    auto u = unit(scopeEndLocation);
                    Map<String, uint16_t> &t = u->targets[scopeEndLocation];
                    t[destructorUsr] = 0;
                    Symbol &scopeEnd = u->symbols[scopeEndLocation];
                    scopeEnd.symbolName = "}";
                    scopeEnd.location = scopeEndLocation;
                    scopeEnd.symbolLength = 1;
                    scopeEnd.flags = Symbol::ImplicitDestruction;
                    scopeEnd.kind = CXCursor_CallExpr;
                    // error() << "Found destructor" << destructor << "for" << cursor << "in scope that ends on" << mScopeStack.back();
                }
                break; }
            default:
                break;
            }
            break; }
        default:
            break;
        }
        break; }
    case CXCursor_EnumConstantDecl:
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 2)
        c.enumValue = clang_getEnumConstantDeclValue(cursor);
#endif
        break;
    case CXCursor_MacroDefinition: {
        CXToken *tokens = nullptr;
        unsigned numTokens = 0;
        clang_tokenize(tu, range, &tokens, &numTokens);
        MacroData &macroData = mMacroTokens[location];
        enum {
            Unset,
            GettingArgs,
            ArgsDone
        } macroState = Unset;
        MacroLocationData *last = nullptr;
        bool lastWasHashHash = false;
        for (size_t i=1; i<numTokens; ++i) {
            const CXTokenKind k = clang_getTokenKind(tokens[i]);
            // error() << i << kind << macroState << RTags::eatString(clang_getTokenSpelling(mTranslationUnit->unit, tokens[i]));
            if (macroState == Unset) {
                if (k == CXToken_Punctuation) {
                    const CXStringScope scope(clang_getTokenSpelling(tu, tokens[i]));
                    if (!strcmp(scope.data(), "("))
                        macroState = GettingArgs;
                }
                if (macroState == Unset)
                    macroState = ArgsDone;
            }
            // error() << i << clang_getTokenKind(tokens[i])
            //         << RTags::eatString(clang_getTokenSpelling(tu, tokens[i]));
            bool isHashHash = false;
            if (k == CXToken_Identifier) {
                const String spelling = RTags::eatString(clang_getTokenSpelling(tu, tokens[i]));
                if (macroState == GettingArgs) {
                    macroData.arguments.append(spelling);
                } else {
                    if (!lastWasHashHash) {
                        last = &macroData.data[spelling];

                        List<Location> &locs = last->locations;
                        locs.append(createLocation(clang_getTokenLocation(tu, tokens[i])));
                    }
                    if (last) {
                        const size_t idx = macroData.arguments.indexOf(spelling);
                        if (idx != String::npos) {
                            last->arguments.insert(idx);
                        }
                    }
                }
            } else if (macroState == GettingArgs && k == CXToken_Punctuation) {
                const CXStringScope scope(clang_getTokenSpelling(tu, tokens[i]));
                if (!strcmp(scope.data(), ")"))
                    macroState = ArgsDone;
            } else if (k == CXToken_Punctuation) {
                const CXStringScope scope(clang_getTokenSpelling(tu, tokens[i]));
                if (!strcmp(scope.data(), "##")) {
                    isHashHash = true;
                } else {
                    last = nullptr;
                }
            } else {
                last = nullptr;
            }
            lastWasHashHash = isHashHash;
        }
        // error() << macroData.arguments;
        // for (const auto &d : macroData.data) {
        //     error() << d.first << d.second.locations << d.second.arguments;
        // }

        clang_disposeTokens(tu, tokens, numTokens);
        break; }
    case CXCursor_FieldDecl:
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 30)
        c.fieldOffset = std::max<int16_t>(-1, clang_Cursor_getOffsetOfField(cursor));
#endif
        break;
    default:
        break;
    }

#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 35)
    if (!(c.flags & Symbol::Auto)
        && kind != CXCursor_LambdaExpr
        && c.type != CXType_LValueReference
        && c.type != CXType_RValueReference
        && c.type != CXType_Auto
        && c.type != CXType_Unexposed) {
        const long long ret = clang_Type_getSizeOf(type);
        if (ret > 0) {
            c.size = static_cast<uint16_t>(ret);
            c.alignment = std::max<int16_t>(-1, clang_Type_getAlignOf(type));
            if (c.size > 0 && (kind == CXCursor_VarDecl || kind == CXCursor_ParmDecl)) {
                for (int i=mScopeStack.size() - 1; i>=0; --i) {
                    auto &scope = mScopeStack.at(i);
                    if (scope.type == Scope::FunctionDefinition) {
                        assert(scope.symbol);
                        scope.symbol->stackCost += c.size;
                        break;
                    } else if (scope.type == Scope::FunctionDeclaration) {
                        break;
                    }
                }
            }
        }
    }
#endif

    c.kind = kind;
    c.linkage = clang_getCursorLinkage(cursor);
    // apparently some function decls will give a different usr for
    // their definition and their declaration.  Using the canonical
    // cursor's usr allows us to join them. Check JSClassRelease in
    // JavaScriptCore for an example.
    unit(location)->usrs[c.usr].insert(location);
    if (c.linkage == CXLinkage_External && !c.isDefinition()) {
        switch (c.kind) {
        case CXCursor_FunctionDecl:
        case CXCursor_VarDecl: {
            const auto k = clang_getCursorKind(clang_getCursorSemanticParent(cursor));
            switch (k) {
            case CXCursor_ClassDecl:
            case CXCursor_ClassTemplate:
            case CXCursor_StructDecl:
                break;
            default:
                unit(location)->targets[location][usr] = RTags::createTargetsValue(k, true);
                break;
            }
            break; }
        default:
            break;
        }
    }

    std::unordered_set<CXCursor> cursors;
    switch (c.kind) {
    case CXCursor_CXXMethod:
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
        if (clang_CXXMethod_isPureVirtual(cursor))
            c.flags |= Symbol::PureVirtualMethod;
        else
#endif
            if (clang_CXXMethod_isVirtual(cursor))
                c.flags |= Symbol::VirtualMethod;

        if (clang_CXXMethod_isStatic(cursor))
            c.flags |= Symbol::StaticMethod;
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 27)
        if (clang_CXXMethod_isConst(cursor))
            c.flags |= Symbol::ConstMethod;
#endif

        cursors = addOverriddenCursors(cursor, location);
        switch (clang_getCursorKind(clang_getCursorSemanticParent(cursor))) {
        case CXCursor_ClassTemplate:
        case CXCursor_ClassTemplatePartialSpecialization:
            c.flags |= Symbol::TemplateFunction;
            break;
        default:
            break;
        }
        RCT_FALL_THROUGH;
    case CXCursor_FunctionDecl:
    case CXCursor_FunctionTemplate:
        if (c.kind == CXCursor_FunctionTemplate)
            c.flags |= Symbol::TemplateFunction;
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 19)
        if (clang_Cursor_isVariadic(cursor))
            c.flags |= Symbol::Variadic;
#endif
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 33)
        if (clang_Cursor_isFunctionInlined(cursor))
            c.flags |= Symbol::InlineFunction;
#endif

        extractArguments(&c.arguments, cursor);
        break;
    case CXCursor_LambdaExpr:
        extractArguments(&c.arguments, cursor);
        break;
    case CXCursor_Constructor:
        extractArguments(&c.arguments, cursor);
        RCT_FALL_THROUGH;
    case CXCursor_Destructor: {
        CXCursor parent = clang_getCursorSemanticParent(cursor);

        switch (clang_getCursorKind(parent)) {
        case CXCursor_ClassTemplate:
        case CXCursor_ClassTemplatePartialSpecialization:
            c.flags |= Symbol::TemplateFunction;
            break;
        default:
            break;
        }

        // these are for joining constructors/destructor with their classes (for renaming symbols)
        assert(!RTags::usr(parent).isEmpty());
        unit(location)->targets[location][RTags::usr(parent)] = 0;
        break; }
    case CXCursor_ClassTemplate:
    case CXCursor_StructDecl:
    case CXCursor_ClassDecl: {
        const CXCursor specialization = clang_getSpecializedCursorTemplate(cursor);
        if (RTags::isValid(specialization)) {
            unit(location)->targets[location][RTags::usr(specialization)] = 0;
            c.flags |= Symbol::TemplateSpecialization;
        }
        break; }
    default:
        break;
    }

    if (!(ClangIndexer::serverOpts() & Server::NoComments)) {
        CXComment comment = clang_Cursor_getParsedComment(cursor);
        if (clang_Comment_getKind(comment) != CXComment_Null) {
            c.briefComment = RTags::eatString(clang_Cursor_getBriefCommentText(cursor));
            c.xmlComment = RTags::eatString(clang_FullComment_getAsXML(comment));
        }

        for (auto it = cursors.begin(); it != cursors.end() && (c.briefComment.isEmpty() || c.xmlComment.isEmpty()); ++it) {
            comment = clang_Cursor_getParsedComment(*it);
            if (clang_Comment_getKind(comment) != CXComment_Null) {
                if (c.briefComment.isEmpty()) {
                    c.briefComment = RTags::eatString(clang_Cursor_getBriefCommentText(*it));
                }
                if (c.xmlComment.isEmpty())
                    c.xmlComment = RTags::eatString(clang_FullComment_getAsXML(comment));
            }
        }
    }

    if (RTags::isFunction(c.kind)) {
        const bool definition = c.flags & Symbol::Definition;
        mScopeStack.append({definition ? Scope::FunctionDefinition : Scope::FunctionDeclaration, definition ? &c : nullptr,
                            Location(location.fileId(), c.startLine, c.startColumn),
                            Location(location.fileId(), c.endLine, c.endColumn - 1)});
        bool isTemplateFunction = c.kind == CXCursor_FunctionTemplate;
        if (!isTemplateFunction  && (c.kind == CXCursor_CXXMethod
                                     || c.kind == CXCursor_Constructor
                                     || c.kind == CXCursor_Destructor)
            && clang_getCursorSemanticParent(cursor) == CXCursor_ClassTemplate) {
            isTemplateFunction = true;
        }
        if (isTemplateFunction)
            ++mInTemplateFunction;
        visit(cursor);
        if (isTemplateFunction)
            --mInTemplateFunction;
        mScopeStack.removeLast();
        return CXChildVisit_Continue;
    }

    return CXChildVisit_Recurse;
}

bool ClangIndexer::parse()
{
    StopWatch sw;
    Flags<Source::CommandLineFlag> commandLineFlags = Source::Default;
    if (ClangIndexer::serverOpts() & Server::PCHEnabled)
        commandLineFlags |= Source::PCHEnabled;

    Flags<CXTranslationUnit_Flags> flags = CXTranslationUnit_DetailedPreprocessingRecord;

    if (mMode == Daemon) {
        flags |= CXTranslationUnit_PrecompiledPreamble;
        flags |= CXTranslationUnit_ForSerialization;
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 32)
        flags |= CXTranslationUnit_CreatePreambleOnFirstParse;
#endif
    }
#if CINDEX_VERSION_MINOR > 33
    flags |= CXTranslationUnit_KeepGoing;
#endif
    bool pch;
    switch (mSources.front().language) {
    case Source::CPlusPlus11Header:
    case Source::CPlusPlusHeader:
    case Source::CHeader:
        flags |= CXTranslationUnit_Incomplete;
        pch = true;
        break;
    default:
        pch = false;
        break;
    }

    List<CXUnsavedFile> unsavedFiles(mUnsavedFiles.size() + 1);
    int unsavedIndex = 0;
    for (const auto &it : mUnsavedFiles) {
        unsavedFiles[unsavedIndex++] = {
            it.first.constData(),
            it.second.constData(),
            static_cast<unsigned long>(it.second.size())
        };
    }

    bool ok = false;
    mTranslationUnits.resize(mSources.size());
    for (size_t idx = 0; idx<mSources.size(); ++idx) {
        const Source &source = mSources.at(idx);
        if (testLog(LogLevel::Debug))
            debug() << "CI::parse: " << source.toCommandLine(commandLineFlags) << "\n";

        // for (const auto it : source.toCommandLine(commandLineFlags)) {
        //     error("[%s]", it.constData());
        // }
        bool usedPch = false;
        const List<String> args = source.toCommandLine(commandLineFlags, &usedPch);
        if (usedPch)
            mIndexDataMessage.setFlag(IndexDataMessage::UsedPCH);

        std::shared_ptr<RTags::TranslationUnit> &unit = mTranslationUnits[idx];
        if (mCachedSources.size() > idx && mCachedSources.at(idx) == source) {
            mFromCache = true;
            assert(mCachedTranslationUnits.size() > idx);
            unit = mCachedTranslationUnits.at(idx);
            warning() << "loaded cached unit for" << mSourceFile;
            assert(unit);
            StopWatch sw2;
            if (!unit->reparse(&unsavedFiles[0], unsavedIndex)) {
                warning() << "Failed to reparse";
                unit.reset();
                mCachedTranslationUnits[idx].reset();
            } else {
                mFromCache = true;
                warning() << "reparsed cached unit in" << sw2.restart();
            }
        }

        if (!unit) {
            unit = RTags::TranslationUnit::create(mSourceFile, args, &unsavedFiles[0], unsavedIndex, flags, false);
            warning() << "CI::parse loading unit:" << unit->clangLine << " " << (unit->unit != nullptr);
        }

        if (unit->unit) {
            if (pch && ClangIndexer::serverOpts() & Server::PCHEnabled) {
                Path path = RTags::encodeSourceFilePath(mDataDir, mProject, source.fileId);
                Path::mkdir(path, Path::Recursive);
                path << "pch.h.gch";
                Path tmp = path;
                tmp << ".tmp";
                clang_saveTranslationUnit(unit->unit, tmp.constData(), clang_defaultSaveOptions(unit->unit));
                rename(tmp.constData(), path.constData());
                warning() << "SAVED PCH" << path;
            }

            ok = true;
            mParseDuration = sw.elapsed();
        } else {
            error() << "Failed to parse" << unit->clangLine;
            mIndexDataMessage.setFlag(IndexDataMessage::ParseFailure);
        }
    }
    if (mMode == Daemon) {
        if (ok) {
            mCachedSources = mSources;
            mCachedTranslationUnits = mTranslationUnits;
        } else {
            mCachedSources = SourceList();
            mCachedTranslationUnits.clear();
        }
    }

    return ok;
}

static inline Map<String, Set<Location> > convertTargets(const Map<Location, Map<String, uint16_t> > &in, bool hasRoot)
{
    Map<String, Set<Location> > ret;
    if (hasRoot) {
        for (const auto &v : in) {
            for (const auto &u : v.second) {
                ret[Sandbox::encoded(u.first)].insert(v.first);
            }
        }
    } else {
        for (const auto &v : in) {
            for (const auto &u : v.second) {
                ret[u.first].insert(v.first);
            }
        }
    }
    return ret;
}

static inline void encodeSymbols(Map<Location, Symbol> &symbols)
{
    assert(Sandbox::hasRoot());
    for (auto &sym : symbols) {
        Sandbox::encode(sym.second.symbolName);
        Sandbox::encode(sym.second.usr);
        Sandbox::encode(sym.second.typeName);
        Sandbox::encode(sym.second.briefComment);
        Sandbox::encode(sym.second.xmlComment);
    }
}

bool ClangIndexer::writeFiles(const Path &root, String &error)
{
    size_t bytesWritten = 0;
    const Path p = Sandbox::encoded(mSourceFile);
    const bool hasRoot = Sandbox::hasRoot();
    const uint32_t fileId = mSources.front().fileId;

    auto process = [&](Hash<uint32_t, std::shared_ptr<Unit> >::const_iterator unit) {
        assert(mIndexDataMessage.files().value(unit->first) & IndexDataMessage::Visited);
        String unitRoot = root;
        unitRoot << unit->first;
        Path::mkdir(unitRoot, Path::Recursive);
        const Path path = Location::path(unit->first);
        if (unit->first != fileId) {
            FILE *f = fopen((unitRoot + "/info").constData(), "w");
            if (!f)
                return false;
            Path rpath = path;
            Sandbox::encode(rpath);
            bytesWritten += fprintf(f, "%s\nIndexed by %s at %llu\n",
                                    rpath.constData(),
                                    p.constData(), static_cast<unsigned long long>(mIndexDataMessage.parseTime()));
            fclose(f);
        }

        auto uit = mUnsavedFiles.find(path);
        if (uit == mUnsavedFiles.end()) {
            Path::rm(unitRoot + "/unsaved");
        } else {
            FILE *f = fopen((unitRoot + "/unsaved").constData(), "w");
            if (!f)
                return false;
            bool ok = fwrite(uit->second.constData(), uit->second.size(), 1, f);
            fclose(f);
            if (!ok)
                return false;
            bytesWritten += uit->second.size();
        }

        //::error() << "Writing file" << Location::path(unit->first) << unitRoot << unit->second->symbols.size()
        //           << unit->second->targets.size()
        //           << unit->second->usrs.size()
        //           << unit->second->symbolNames.size();
        uint32_t fileMapOpts = 0;
        if (ClangIndexer::serverOpts() & Server::NoFileLock)
            fileMapOpts |= FileMap<int, int>::NoLock;

        if (hasRoot) {
            encodeSymbols(unit->second->symbols);
            Sandbox::encode(unit->second->usrs);
            Sandbox::encode(unit->second->symbolNames);
        }

        size_t w;
        // for (const char *name : { "/symbols", "/targets", "/usrs", "/symnames", "/tokens" }) {
        //     if (Path::exists(unitRoot + "/symbols"))
        //         ::error() << (unitRoot + name) << "already exists";
        // }
        if (!(w = FileMap<Location, Symbol>::write(unitRoot + "/symbols", unit->second->symbols, fileMapOpts))) {
            error = "Failed to write symbols";
            return false;
        }
        bytesWritten += w;

        if (!(w = FileMap<String, Set<Location> >::write(unitRoot + "/targets", convertTargets(unit->second->targets, hasRoot), fileMapOpts))) {
            error = "Failed to write targets";
            return false;
        }
        bytesWritten += w;

        if (!(w += FileMap<String, Set<Location> >::write(unitRoot + "/usrs", unit->second->usrs, fileMapOpts))) {
            error = "Failed to write usrs";
            return false;
        }
        bytesWritten += w;

        if (!(w += FileMap<String, Set<Location> >::write(unitRoot + "/symnames", unit->second->symbolNames, fileMapOpts))) {
            error = "Failed to write symbolNames";
            return false;
        }
        bytesWritten += w;

        if (!(w += FileMap<uint32_t, Token>::write(unitRoot + "/tokens", unit->second->tokens, fileMapOpts))) {
            error = "Failed to write symbolNames";
            return false;
        }
        bytesWritten += w;
        return true;
    };

    List<std::shared_ptr<Unit> > templateSpecializationTargets;
    auto self = mUnits.end();
    for (auto it = mUnits.begin(); it != mUnits.end(); ++it) {
        if (!(mIndexDataMessage.files().value(it->first) & IndexDataMessage::Visited)) {
            ::error() << "Wanting to write something for"
                      << it->first << Location::path(it->first)
                      << "but we didn't visit it" << mSourceFile
                      << "targets" << it->second->targets.size()
                      << "usrs" << it->second->usrs.size()
                      << "symbolNames" << it->second->symbolNames.size()
                      << "symbols" << it->second->symbols.size()
                      << "tokens" << it->second->tokens.size();
            continue;
        }
        if (it->first == fileId) {
            self = it;
        } else if (!process(it)) {
            return false;
        }
    }

    if (self != mUnits.end()) {
        for (const std::shared_ptr<Unit> &t : templateSpecializationTargets) {
            self->second->targets.unite(t->targets);
        }
        if (!process(self)) {
            return false;
        }
    }
    String sourceRoot = root;
    sourceRoot << fileId;
    Path::mkdir(sourceRoot, Path::Recursive);
    sourceRoot << "/info";
    FILE *f = fopen(sourceRoot.constData(), "w");
    if (!f) {
        return false;
    }

    for (const Source &source : mSources) {
        const String args = Sandbox::encoded(String::join(source.toCommandLine(Source::Default|Source::IncludeCompiler|Source::IncludeSourceFile), ' '));

        bytesWritten += fprintf(f, "%s\n%s\n", p.constData(), args.constData());
    }
    bytesWritten += fprintf(f, "Indexed at %llu\n", static_cast<unsigned long long>(mIndexDataMessage.parseTime()));

    fclose(f);
    mIndexDataMessage.setBytesWritten(bytesWritten);
    return true;
}

bool ClangIndexer::diagnose()
{
    DiagnosticsProvider::diagnose();
    for (size_t i=0; i<mTranslationUnits.size(); ++i) {
        mCurrentTranslationUnit = i;
        auto tu = mTranslationUnits.at(mCurrentTranslationUnit)->unit;
        if (!tu) {
            continue;
        }
        for (const auto &it : mIndexDataMessage.files()) {
            if (it.second & IndexDataMessage::Visited) {
                const Location loc(it.first, 0, 0);
                const Path path = loc.path();
                CXFile file = clang_getFile(tu, path.constData());
                if (file) {
                    tokenize(file, it.first, path);
                }
            }
        }
    }

    return true;
}

void ClangIndexer::tokenize(CXFile file, uint32_t fileId, const Path &path)
{
    const auto &tu = mTranslationUnits.at(mCurrentTranslationUnit)->unit;
    StopWatch sw;
    const CXSourceLocation startLoc = clang_getLocationForOffset(tu, file, 0);
    const CXSourceLocation endLoc = clang_getLocationForOffset(tu, file, path.fileSize());

    CXSourceRange range = clang_getRange(startLoc, endLoc);
    CXToken *tokens = nullptr;
    unsigned numTokens = 0;
    auto &map = unit(fileId)->tokens;
    clang_tokenize(tu, range, &tokens, &numTokens);
    for (unsigned i=0; i<numTokens; ++i) {
        range = clang_getTokenExtent(tu, tokens[i]);
        unsigned offset, endOffset;
        const CXSourceLocation start = clang_getRangeStart(range);
        clang_getSpellingLocation(start, nullptr, nullptr, nullptr, &offset);
        clang_getSpellingLocation(clang_getRangeEnd(range), nullptr, nullptr, nullptr, &endOffset);
        map[offset] = {
            clang_getTokenKind(tokens[i]),
            RTags::eatString(clang_getTokenSpelling(tu, tokens[i])),
            createLocation(start),
            offset,
            endOffset - offset
        };
    }

    clang_disposeTokens(tu, tokens, numTokens);
}

bool ClangIndexer::visit()
{
    StopWatch watch;
    for (size_t i=0; i<mTranslationUnits.size(); ++i) {
        mCurrentTranslationUnit = i;
        const auto &unit = mTranslationUnits.at(mCurrentTranslationUnit);
        assert(mSources.front().fileId);
        if (!unit->unit) {
            continue;
        }

        visit(clang_getTranslationUnitCursor(unit->unit));

        if (testLog(LogLevel::VerboseDebug)) {
            VerboseVisitorUserData u = { 0, "<VerboseVisitor " + unit->clangLine + ">\n", this };
            clang_visitChildren(clang_getTranslationUnitCursor(unit->unit),
                                ClangIndexer::verboseVisitor, &u);
            u.out += "</VerboseVisitor " + unit->clangLine + ">";
            if (getenv("RTAGS_INDEXERJOB_DUMP_TO_FILE")) {
                char buf[1024];
                snprintf(buf, sizeof(buf), "/tmp/%s.log", mSourceFile.fileName());
                FILE *f = fopen(buf, "w");
                assert(f);
                fwrite(u.out.constData(), 1, u.out.size(), f);
                fclose(f);
            } else {
                logDirect(LogLevel::VerboseDebug, u.out);
            }
        }
    }

    for (const auto &it : mIndexDataMessage.files()) {
        if (it.second & IndexDataMessage::Visited)
            addFileSymbol(it.first);
    }

    std::unordered_set<CXCursor> seen;
    for (const CXCursor &spec : mTemplateSpecializations) {
        std::function<CXChildVisitResult(CXCursor)> visitor = [&visitor, &seen, this](CXCursor cursor) {
            if (!seen.insert(cursor).second) {
                return CXChildVisit_Continue;
            }
            const CXCursorKind kind = clang_getCursorKind(cursor);
            const CXCursor ref = clang_getCursorReferenced(cursor);
            if (!ref)
                return CXChildVisit_Recurse;
            const CXCursorKind refKind = clang_getCursorKind(ref);
            if (kind == CXCursor_CallExpr && (refKind == CXCursor_CXXMethod
                                              || refKind == CXCursor_FunctionDecl
                                              || refKind == CXCursor_FunctionTemplate)) {
                return CXChildVisit_Recurse;
            }

            // error() << "considering" << cursor << "for" << ref;
            bool ignored;
            const Location loc = createLocation(cursor, kind, &ignored);
            if (!loc.isNull()) {
                const String refUsr = RTags::usr(resolveTemplateUsr(resolveTemplate(ref)));
                if (!refUsr.isEmpty()) {
                    assert(!refUsr.isEmpty());
                    const uint32_t fileId = mSources.front().fileId;
                    unit(fileId)->targets[loc][refUsr] = RTags::createTargetsValue(refKind, clang_isCursorDefinition(ref));
                }
                if (RTags::isFunction(refKind) && mTemplateSpecializations.find(ref) == mTemplateSpecializations.end()) {
                    RTags::TranslationUnit::visit(ref, visitor);
                }
                // error() << "inserting" << loc << refUsr;
            }
            return CXChildVisit_Recurse;
        };
        RTags::TranslationUnit::visit(spec, visitor);
    }

    mVisitDuration = watch.elapsed();

    return true;
}

CXChildVisitResult ClangIndexer::verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
    Location loc = u->indexer->createLocation(cursor);
    if (loc.fileId()) {
        CXCursor ref = clang_getCursorReferenced(cursor);

        if (u->indent >= 0)
            u->out += String(u->indent, ' ');
        u->out += RTags::cursorToString(cursor);
        if (ref == cursor) {
            u->out += " refs self";
        } else if (RTags::isValid(ref)) {
            u->out += " refs " + RTags::cursorToString(ref);
        }

        if (loc.fileId() && u->indexer->mIndexDataMessage.files().value(loc.fileId()) & IndexDataMessage::Visited) {
            if (u->indexer->unit(loc)->symbols.contains(loc)) {
                u->out += " used as cursor\n";
            } else {
                u->out += " not used\n";
            }
        } else {
            u->out += " not indexed\n";
        }
    }
    if (u->indent >= 0) {
        u->indent += 2;
        clang_visitChildren(cursor, ClangIndexer::verboseVisitor, userData);
        u->indent -= 2;
        return CXChildVisit_Continue;
    } else {
        return CXChildVisit_Recurse;
    }
}

void ClangIndexer::addFileSymbol(uint32_t file)
{
    const Location loc(file, 1, 1);
    const Path path = Location::path(file);
    auto ref = unit(loc);
    ref->symbolNames[path].insert(loc);
    const char *fn = path.fileName();
    ref->symbolNames[fn].insert(loc);
    Symbol &sym = ref->symbols[loc];
    if (sym.isNull())
        sym.flags |= Symbol::FileSymbol;
    sym.location = loc;
}

int ClangIndexer::symbolLength(CXCursorKind kind, const CXCursor &cursor)
{
    if (kind == CXCursor_VarDecl && RTags::resolveAuto(cursor)) {
        return 4;
    }

    CXStringScope name = clang_getCursorSpelling(cursor);
    const char *cstr = name.data();
    if (cstr)
        return strlen(cstr);

    // this is for these constructs:
    //         ||
    //         \/
    // typedef struct {
    //    int a;
    // } foobar;
    //
    // We end up not getting a spelling for the cursor

    switch (kind) {
    case CXCursor_EnumDecl:
        return 4;
    case CXCursor_ClassDecl:
    case CXCursor_UnionDecl:
        return 5;
    case CXCursor_StructDecl:
        return 6;
    default:
        break;
    }
    return 0;
}

Symbol ClangIndexer::findSymbol(Location location, FindResult *result) const
{
    auto it = mUnits.find(location.fileId());
    if (it != mUnits.end()) {
        bool ok;
        Symbol ret = it->second->symbols.value(location, Symbol(), &ok);
        if (ok) {
            *result = Found;
            return ret;
        }
    }

    if (mIndexDataMessage.files().value(location.fileId()) & IndexDataMessage::Visited) {
        *result = NotFound;
    } else {
        *result = NotIndexed;
    }
    return Symbol();
}

CXCursor ClangIndexer::resolveTemplate(CXCursor cursor, Location location, bool *specialized)
{
    if (specialized)
        *specialized = false;
    while (true) {
        const CXCursor general = clang_getSpecializedCursorTemplate(cursor);
        if (clang_Cursor_isNull(general))
            break;
        if (specialized)
            *specialized = true;
        if (location.isNull())
            location = createLocation(cursor);
        if (createLocation(general) == location) {
            cursor = general;
        } else {
            break;
        }
    }
    return cursor;
}

CXCursor ClangIndexer::resolveTemplateUsr(const CXCursor cursor) const
{
    // Replace specialized instance of fieldDecls or cast operators etc so
    // we can properly reference them
    CXCursor c = cursor;
    CXCursorKind kind;
    while (true) {
        kind = clang_getCursorKind(c);
        if (clang_isInvalid(kind))
            break;

        if ((kind == CXCursor_StructDecl
             || kind == CXCursor_ClassDecl
             || kind == CXCursor_ClassTemplate
             || kind == CXCursor_ClassTemplatePartialSpecialization)
            && !clang_isInvalid(clang_getCursorKind(clang_getSpecializedCursorTemplate(c)))) {
            break;
        }
        c = clang_getCursorSemanticParent(c);
    }

    if (!clang_isInvalid(kind)) {
        CXSourceLocation sourceLoc = clang_getCursorLocation(cursor);
        c = clang_getCursor(mTranslationUnits.at(mCurrentTranslationUnit)->unit, sourceLoc);
        if (c != cursor && !clang_isInvalid(clang_getCursorKind(c))) {
            return c;
        }
    }
    return cursor;
}

CXCursor ClangIndexer::resolveTypedef(CXCursor cursor)
{
    while (clang_getCursorKind(cursor) == CXCursor_TypedefDecl) {
        CXCursor typedeffed = clang_getTypeDeclaration(clang_getTypedefDeclUnderlyingType(cursor));
        if (RTags::isValid(typedeffed)) {
            cursor = typedeffed;
        } else {
            break;
        }
    }
    return cursor;
}
