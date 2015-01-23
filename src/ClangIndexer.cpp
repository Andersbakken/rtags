/* This file is part of RTags.

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

#define RTAGS_SINGLE_THREAD
#include <rct/SHA256.h>
#include "ClangIndexer.h"
#include "Diagnostic.h"
#include "QueryMessage.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include "FileMap.h"
#include <rct/Connection.h>
#include <rct/EventLoop.h>
#include "RTags.h"
#include "Diagnostic.h"
#include "IndexerMessage.h"
#include "IndexData.h"
#include "RClient.h"
#include <unistd.h>

static const CXSourceLocation nullLocation = clang_getNullLocation();
static const CXCursor nullCursor = clang_getNullCursor();

struct VerboseVisitorUserData {
    int indent;
    String out;
    ClangIndexer *indexer;
};

ClangIndexer::ClangIndexer()
    : mClangUnit(0), mIndex(0), mLastCursor(nullCursor), mVisitFileResponseMessageFileId(0),
      mVisitFileResponseMessageVisit(0), mParseDuration(0), mVisitDuration(0),
      mBlocked(0), mAllowed(0), mIndexed(1), mVisitFileTimeout(0),
      mIndexerMessageTimeout(0), mFileIdsQueried(0), mLogFile(0), mConnection(RClient::NumOptions)
{
    mConnection.newMessage().connect(std::bind(&ClangIndexer::onMessage, this,
                                               std::placeholders::_1, std::placeholders::_2));
}

ClangIndexer::~ClangIndexer()
{
    if (mLogFile)
        fclose(mLogFile);
    if (mClangUnit)
        clang_disposeTranslationUnit(mClangUnit);
    if (mIndex)
        clang_disposeIndex(mIndex);
}

bool ClangIndexer::exec(const String &data)
{
    Deserializer deserializer(data);
    uint16_t protocolVersion;
    deserializer >> protocolVersion;
    if (protocolVersion != RTags::DatabaseVersion) {
        error("Wrong protocol %d vs %d", protocolVersion, RTags::DatabaseVersion);
        return false;
    }
    uint64_t id;
    String socketFile;
    uint32_t flags;
    uint32_t connectTimeout;
    int32_t niceValue;
    extern bool suspendOnSigSegv;
    Hash<uint32_t, Path> blockedFiles;
    String dataDir;

    deserializer >> id;
    deserializer >> socketFile;
    deserializer >> mProject;
    deserializer >> mSource;
    deserializer >> mSourceFile;
    deserializer >> flags;
    deserializer >> mVisitFileTimeout;
    deserializer >> mIndexerMessageTimeout;
    deserializer >> connectTimeout;
    deserializer >> niceValue;
    deserializer >> suspendOnSigSegv;
    deserializer >> mUnsavedFiles;
    deserializer >> dataDir;

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

    uint32_t dirtySize;
    deserializer >> dirtySize;
    const uint64_t parseTime = Rct::currentTimeMs();

    while (dirtySize-- > 0) {
        Path dirty;
        deserializer >> dirty;
        if (!mUnsavedFiles.contains(dirty)) {
            mUnsavedFiles[dirty] = dirty.readAll();
        }
    }

    deserializer >> blockedFiles;

    if (niceValue != INT_MIN) {
        errno = 0;
        if (nice(niceValue) == -1) {
            error() << "Failed to nice rp" << strerror(errno);
        }
    }

    if (mSourceFile.isEmpty()) {
        error("No sourcefile");
        return false;
    }
    if (!mSource.fileId) {
        error("Bad fileId");
        return false;
    }

    if (mProject.isEmpty()) {
        error("No project");
        return false;
    }

    Location::init(blockedFiles);
    Location::set(mSourceFile, mSource.fileId);
    if (!mConnection.connectUnix(socketFile, connectTimeout)) {
        error("Failed to connect to rdm on %s (%dms timeout)", socketFile.constData(), connectTimeout);
        return false;
    }
    // mLogFile = fopen(String::format("/tmp/%s", mSourceFile.fileName()).constData(), "w");
    mData.reset(new IndexData(flags));
    mData->parseTime = parseTime;
    mData->key = mSource.key();
    mData->id = id;

    assert(mConnection.isConnected());
    mData->visited[mSource.fileId] = true;
    parse() && visit() && diagnose();
    mData->message = mSourceFile.toTilde();
    String err;
    StopWatch sw;
    int writeDuration = -1;
    if (!mClangUnit || !writeFiles(RTags::encodeSourceFilePath(dataDir, mProject, 0), err)) {
        mData->message += " error";
        if (!err.isEmpty())
            mData->message += (' ' + err);
    } else {
        writeDuration = sw.elapsed();
    }
    mData->message += String::format<16>(" in %lldms. ", mTimer.elapsed());
    int cursorCount = 0;
    int symbolNameCount = 0;
    for (const auto &unit : mUnits) {
        cursorCount += unit.second->cursors.size();
        symbolNameCount += unit.second->symbolNames.size();
    }
    if (mClangUnit) {
        const char *format = "(%d syms, %d symNames, %d deps, %d of %d files, cursors: %d of %d, %d queried) (%d/%d/%dms)";
        mData->message += String::format<128>(format, cursorCount, symbolNameCount,
                                              mData->dependencies.size(), mIndexed, mData->visited.size(), mAllowed,
                                              mAllowed + mBlocked, mFileIdsQueried,
                                              mParseDuration, mVisitDuration, writeDuration);
    } else if (mData->dependencies.size()) {
        mData->message += String::format<16>("(%d deps)", mData->dependencies.size());
    }
    if (mData->flags & IndexerJob::Dirty)
        mData->message += " (dirty)";
    const IndexerMessage msg(mProject, mData);
    ++mFileIdsQueried;

    sw.restart();
    if (!mConnection.send(msg)) {
        error() << "Couldn't send IndexerMessage" << mSourceFile;
        return false;
    }
    mConnection.finished().connect(std::bind(&EventLoop::quit, EventLoop::eventLoop()));
    if (EventLoop::eventLoop()->exec(mIndexerMessageTimeout) == EventLoop::Timeout) {
        error() << "Timed out sending IndexerMessage" << mSourceFile;
        return false;
    }
    if (getenv("RDM_DEBUG_INDEXERMESSAGE"))
        error() << "Send took" << sw.elapsed() << "for" << mSourceFile;

    return true;
}

void ClangIndexer::onMessage(const std::shared_ptr<Message> &msg, Connection */*conn*/)
{
    assert(msg->messageId() == VisitFileResponseMessage::MessageId);
    const std::shared_ptr<VisitFileResponseMessage> vm = std::static_pointer_cast<VisitFileResponseMessage>(msg);
    mVisitFileResponseMessageVisit = vm->visit();
    mVisitFileResponseMessageFileId = vm->fileId();
    assert(EventLoop::eventLoop());
    EventLoop::eventLoop()->quit();
}

Location ClangIndexer::createLocation(const Path &sourceFile, unsigned line, unsigned col, bool *blockedPtr)
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
            Hash<uint32_t, bool>::iterator it = mData->visited.find(id);
            if (it == mData->visited.end()) {
                // the only reason we already have an id for a file that isn't
                // in the mData->visited is that it's blocked from the outset.
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
                mData->visited[id] = false;
                *blockedPtr = true;
                return Location();
            } else if (!it->second) {
                *blockedPtr = true;
                return Location();
            }
        }
        return Location(id, line, col);
    }

    ++mFileIdsQueried;
    VisitFileMessage msg(resolved, mProject, mData->key);

    mVisitFileResponseMessageFileId = UINT_MAX;
    mVisitFileResponseMessageVisit = false;
    mConnection.send(msg);
    StopWatch sw;
    EventLoop::eventLoop()->exec(mVisitFileTimeout);
    switch (mVisitFileResponseMessageFileId) {
    case 0:
        return Location();
    case UINT_MAX:
        // timed out.
        if (mVisitFileResponseMessageFileId == UINT_MAX) {
            error() << "Error getting fileId for" << resolved << mLastCursor
                    << sw.elapsed() << mVisitFileTimeout;
        }
        exit(1);
    default:
        id = mVisitFileResponseMessageFileId;
        break;
    }
    mData->visited[id] = mVisitFileResponseMessageVisit;
    if (mVisitFileResponseMessageVisit)
        ++mIndexed;
    // fprintf(mLogFile, "%s %s\n", file.second ? "WON" : "LOST", resolved.constData());

    Location::set(resolved, id);
    if (resolved != sourceFile)
        Location::set(sourceFile, id);

    if (blockedPtr && !mVisitFileResponseMessageVisit) {
        *blockedPtr = true;
        return Location();
    }
    return Location(id, line, col);
}

static inline void tokenize(const char *buf, int start,
                            int *templateStart, int *templateEnd,
                            int *sectionCount, int sections[512])
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
            if (!templateCount && (functionStart == -1 || functionEnd != -1)) {
                if (buf[idx + 1] == ':') {
                    sections[(*sectionCount)++] = idx + 2;
                    ++idx;
                } else {
                    sections[(*sectionCount)++] = idx + 1;
                }
            }
            break;
        case '\0':
            return;
        }
    }
}

String ClangIndexer::addNamePermutations(const CXCursor &cursor, const Location &location, String type)
{
    CXCursorKind kind = clang_getCursorKind(cursor);
    const CXCursorKind originalKind = kind;
    char buf[32768];
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

        if (pos != sizeof(buf) - 1 && (pos -= 2) >= 0) {
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
            default:
                cutoff = pos;
                break;
            }
        }
    } while (RTags::needsQualifiers(kind));

    if (type.isEmpty()) {
        switch (originalKind) {
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
        case CXCursor_ClassTemplate:
            break;
        default:
            type = RTags::typeName(cursor);
            break;
        }
    } else if (!type.isEmpty() && !type.endsWith('*') && !type.endsWith('&')) {
        type.append(' ');
    }

    if (cutoff == -1)
        cutoff = pos;

    String ret;
    int templateStart, templateEnd, colonColonCount;
    int colonColons[512];
    ::tokenize(buf, pos,
               &templateStart, &templateEnd,
               &colonColonCount, colonColons);

    // i == 0 --> with templates,
    // i == 1 without templates or without EnumConstantDecl part
    for (int i=0; i<2; ++i) {
        for (int j=0; j<colonColonCount; ++j) {
            const char *ch = buf + colonColons[j];
            const String name(ch, sizeof(buf) - (ch - buf) - 1);
            unit(location.fileId())->symbolNames[name].insert(location);
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

        if (i == 0) {
            // create actual symbol name that will go into CursorInfo. This doesn't include namespaces etc
            if (!type.isEmpty()) {
                ret = type;
                ret.append(buf + cutoff, sizeof(buf) - cutoff - 1);
            } else {
                ret.assign(buf + cutoff, sizeof(buf) - cutoff - 1);
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
                // ### We could/should just move the colon colon values but this
                // should be pretty quick and I don't want to write the code to
                // do it.
                ::tokenize(buf, pos,
                           &templateStart, &templateEnd,
                           &colonColonCount, colonColons);
            }
        } else { // remove templates
            assert(templateStart != -1);
            assert(templateEnd != -1);
            const int templateSize = (templateEnd - templateStart) + 1;
            memmove(buf + pos + templateSize, buf + pos, (buf + templateStart) - (buf + pos));
            pos += templateSize;
        }
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
        return nullCursor;
    }

    const CXCursor var = clang_getCursorReferenced(child);
    kind = clang_getCursorKind(var);
    switch (kind) {
    case CXCursor_VarDecl:
    case CXCursor_FieldDecl:
    case CXCursor_ParmDecl:
    case CXCursor_CXXMethod:
    case CXCursor_FunctionDecl:
        break;
    default:
        if (!clang_isInvalid(kind)) {
            error() << "Got unexpected cursor" << deleteStatement << var;
            // assert(0);
        }
        return nullCursor;
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
        return nullCursor;
    }

    const CXCursor referenced = clang_getCursorReferenced(ref);
    kind = clang_getCursorKind(referenced);
    switch (kind) {
    case CXCursor_StructDecl:
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
        break;
    default:
        return nullCursor;
    }
    const CXCursor destructor = RTags::findChild(referenced, CXCursor_Destructor);
    return destructor;
}

struct LastCursorUpdater
{
    LastCursorUpdater(CXCursor &var, const CXCursor &cursor) : mVar(var), mCursor(cursor) {}
    ~LastCursorUpdater() { mVar = mCursor; }

    CXCursor &mVar;
    CXCursor mCursor;
};

CXChildVisitResult ClangIndexer::indexVisitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
    ClangIndexer *indexer = static_cast<ClangIndexer*>(data);
    // error() << "indexVisitor" << cursor;
    // FILE *f = fopen("/tmp/clangindex.log", "a");
    // String str;
    // Log(&str) << cursor;
    // fwrite(str.constData(), 1, str.size(), f);
    // fwrite("\n", 1, 1, f);
    // fclose(f);
    const LastCursorUpdater updater(indexer->mLastCursor, cursor);

    const CXCursorKind kind = clang_getCursorKind(cursor);
    const RTags::CursorType type = RTags::cursorType(kind);
    if (type == RTags::Type_Other)
        return CXChildVisit_Recurse;

    bool blocked = false;

    Location loc = indexer->createLocation(cursor, &blocked);
    if (blocked) {
        // error() << "blocked" << cursor;
        ++indexer->mBlocked;
        return CXChildVisit_Continue;
    } else if (loc.isNull()) {
        // error() << "Got null" << cursor;
        return CXChildVisit_Recurse;
    }
    ++indexer->mAllowed;
    if (indexer->mLogFile) {
        String out;
        Log(&out) << cursor;
        fwrite(out.constData(), 1, out.size(), indexer->mLogFile);
        fwrite("\n", 1, 1, indexer->mLogFile);
    }

    if (testLog(VerboseDebug)) {
        Log log(VerboseDebug);
        log << cursor;
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (!clang_isInvalid(clang_getCursorKind(ref)) && !clang_equalCursors(ref, cursor)) {
            log << "refs" << ref;
        }
    }
    switch (type) {
    case RTags::Type_Cursor:
        indexer->handleCursor(cursor, kind, loc);
        break;
    case RTags::Type_Include:
        indexer->handleInclude(cursor, kind, loc);
        break;
    case RTags::Type_Reference:
        switch (kind) {
        case CXCursor_OverloadedDeclRef: {
            const int count = clang_getNumOverloadedDecls(cursor);
            for (int i=0; i<count; ++i) {
                const CXCursor ref = clang_getOverloadedDecl(cursor, i);
                indexer->handleReference(cursor, kind, loc, ref, parent);
            }
            break; }
        case CXCursor_CXXDeleteExpr:
            indexer->handleReference(cursor, kind, loc, findDestructorForDelete(cursor), parent);
            break;
        case CXCursor_CallExpr: {
            // uglehack, see rtags/tests/nestedClassConstructorCallUgleHack/
            const CXCursor ref = clang_getCursorReferenced(cursor);
            if (clang_getCursorKind(ref) == CXCursor_Constructor
                && clang_getCursorKind(indexer->mLastCursor) == CXCursor_TypeRef
                && clang_getCursorKind(parent) != CXCursor_VarDecl) {
                loc = indexer->createLocation(indexer->mLastCursor);
                indexer->handleReference(indexer->mLastCursor, kind, loc, ref, parent);
            } else {
                indexer->handleReference(cursor, kind, loc, ref, parent);
            }
            break; }
        default:
            indexer->handleReference(cursor, kind, loc, clang_getCursorReferenced(cursor), parent);
            break;
        }
        break;
    case RTags::Type_Other:
        assert(0);
        break;
    }
    return CXChildVisit_Recurse;
}

static inline bool isImplicit(const CXCursor &cursor)
{
    return clang_equalLocations(clang_getCursorLocation(cursor),
                                clang_getCursorLocation(clang_getCursorSemanticParent(cursor)));
}

bool ClangIndexer::superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                            const Location &location, const CXCursor &/*ref*/,
                                                            const CXCursor &parent, Cursor **cursorPtr)
{
    // This is for references to superclass template functions. Awful awful
    // shit. See https://github.com/Andersbakken/rtags/issues/62 and commit
    // for details. I really should report this as a bug.
    if (cursorPtr)
        *cursorPtr = 0;
    if (kind != CXCursor_MemberRefExpr && clang_getCursorKind(parent) != CXCursor_CallExpr)
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
    unsigned offset;
    clang_getSpellingLocation(end, 0, 0, 0, &offset);

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
        const int argCount = RTags::children(parent, RTags::Filter(), out).size();
        RTags::Filter in(RTags::Filter::And);
        in.names.insert(name);
        in.argumentCount = argCount;
        const List<CXCursor> alternatives = RTags::children(classTemplate, in);
        switch (alternatives.size()) {
        case 1:
            // ### not sure this is correct with line/col
            return handleReference(cursor, kind,
                                   Location(location.fileId(), location.line(), location.column() + 1),
                                   alternatives.first(), parent, cursorPtr);
            break;
        case 0:
            break;
        default:
            warning() << "Can't decide which of these cursors are right for me"
                      << cursor << alternatives
                      << "Need to parse types";
            break;
        }
    }
    return false;
}

bool ClangIndexer::handleReference(const CXCursor &cursor, CXCursorKind kind,
                                   const Location &location, const CXCursor &ref,
                                   const CXCursor &parent, Cursor **cursorPtr)
{
    if (cursorPtr)
        *cursorPtr = 0;
    // error() << "handleReference" << cursor << kind << location << ref;
    const CXCursorKind refKind = clang_getCursorKind(ref);
    if (clang_isInvalid(refKind)) {
        return superclassTemplateMemberFunctionUgleHack(cursor, kind, location, ref, parent, cursorPtr);
    }

    bool isOperator = false;
    if (kind == CXCursor_CallExpr && (refKind == CXCursor_CXXMethod
                                      || refKind == CXCursor_ConversionFunction
                                      || refKind == CXCursor_FunctionDecl
                                      || refKind == CXCursor_FunctionTemplate)) {
        // these are bullshit, for this construct:
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

    switch (refKind) {
    case CXCursor_Constructor:
        if (isImplicit(ref))
            return false;
        break;
    case CXCursor_CXXMethod:
    case CXCursor_FunctionDecl:
    case CXCursor_FunctionTemplate: {
        CXStringScope scope = clang_getCursorDisplayName(ref);
        const char *data = scope.data();
        if (data) {
            const int len = strlen(data);
            if (len > 8 && !strncmp(data, "operator", 8) && !isalnum(data[8]) && data[8] != '_') {
                if (isImplicit(ref))
                    return false; // eat implicit operator calls
                isOperator = true;
            }
        }
        break; }
    default:
        break;
    }

    const Location reffedLoc = createLocation(ref);
    if (!reffedLoc.isValid()) {
        // ### THIS IS NOT SOLVED
        // if (kind == CXCursor_ObjCMessageExpr) {
        //    mData->pendingReferenceMap[RTags::eatString(clang_getCursorUSR(clang_getCanonicalCursor(ref)))].insert(location);
        //     // insert it, we'll hook up the target and references later
        //     return handleCursor(cursor, kind, location, cursorPtr);
        // }
        return false;
    }

    bool reffedCursorFound;
    auto reffedCursor = findCursor(reffedLoc, &reffedCursorFound);
    Map<Location, uint16_t> &targets = unit(location.fileId())->targets[location];
    uint16_t refTargetValue;
    if (reffedCursorFound) {
        refTargetValue = reffedCursor.targetsValue();
    } else {
        refTargetValue = RTags::createTargetsValue(refKind, clang_isCursorDefinition(ref));
    }
    targets[reffedLoc] = refTargetValue;
    Cursor &c = unit(location)->cursors[location];
    if (cursorPtr)
        *cursorPtr = &c;

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

    if (!c.isNull()) {
        if (RTags::isCursor(c.kind))
            return true;
        auto best = targets.end();
        int bestRank = -1;
        for (auto it = targets.begin(); it != targets.end(); ++it) {
            const int r = RTags::targetRank(RTags::targetsValueKind(it->second));
            if (r > bestRank || (r == bestRank && RTags::targetsValueIsDefinition(it->second))) {
                bestRank = r;
                best = it;
            }
        }
        if (best != targets.end() && best->first != location) // another target is better
            return true;
    }


    CXSourceRange range = clang_getCursorExtent(cursor);
    CXSourceLocation rangeStart = clang_getRangeStart(range);
    CXSourceLocation rangeEnd = clang_getRangeEnd(range);
    unsigned startLine, startColumn, endLine, endColumn;
    clang_getSpellingLocation(rangeStart, 0, &startLine, &startColumn, 0);
    clang_getSpellingLocation(rangeEnd, 0, &endLine, &endColumn, 0);
    c.startLine = startLine;
    c.endLine = endLine;
    c.startColumn = startColumn;
    c.endColumn = endColumn;
    c.definition = false;
    c.kind = kind;
    c.location = location;
    if (isOperator) {
        unsigned start, end;
        clang_getSpellingLocation(rangeStart, 0, 0, 0, &start);
        clang_getSpellingLocation(rangeEnd, 0, 0, 0, &end);
        c.symbolLength = end - start;
    } else {
        c.symbolLength = reffedCursorFound ? reffedCursor.symbolLength : symbolLength(refKind, ref);
    }
    if (!c.symbolLength) {
        unit(location)->cursors.remove(location);
        if (cursorPtr)
            *cursorPtr = 0;
        return false;
    }
    c.type = clang_getCursorType(cursor).kind;

    return true;
}


void ClangIndexer::addOverriddenCursors(const CXCursor &cursor, const Location &location, List<Location> &locations)
{
    CXCursor *overridden;
    unsigned count;
    clang_getOverriddenCursors(cursor, &overridden, &count);
    if (!overridden)
        return;
    for (unsigned i=0; i<count; ++i) {
        const Location loc = createLocation(overridden[i]);
        if (loc.isNull())
            continue;

        //error() << "adding overridden (1) " << location << " to " << o;
        const uint16_t targetsValue = RTags::createTargetsValue(overridden[i]);
        unit(location)->targets[location][loc] = targetsValue;
        for (const auto &l : locations) {
            unit(loc)->targets[loc][l] = targetsValue;
        }

        locations.append(loc);
        addOverriddenCursors(overridden[i], loc, locations);
        locations.removeLast();
    }
    clang_disposeOverriddenCursors(overridden);
}

void ClangIndexer::handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location)
{
    assert(kind == CXCursor_InclusionDirective);
    (void)kind;
    CXFile includedFile = clang_getIncludedFile(cursor);
    if (includedFile) {
        const Location refLoc = createLocation(includedFile, 1, 1);
        if (!refLoc.isNull()) {
            Cursor &c = unit(location)->cursors[location];
            if (!c.isNull())
                return;

            String include = "#include ";
            const Path path = refLoc.path();
            assert(mSource.fileId);
            mData->dependencies[refLoc.fileId()].insert(mSource.fileId);
            unit(location.fileId())->symbolNames[(include + path)].insert(location);
            unit(location.fileId())->symbolNames[(include + path.fileName())].insert(location);
            c.symbolName = "#include " + RTags::eatString(clang_getCursorDisplayName(cursor));
            c.kind = cursor.kind;
            c.symbolLength = c.symbolName.size() + 2;
            c.location = location;
            unit(location)->targets[location][refLoc] = 0; // ### what targets value to create for this?
            // this fails for things like:
            // # include    <foobar.h>
        }
    }
}

bool ClangIndexer::handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location, Cursor **cursorPtr)
{
    // error() << "Got a cursor" << cursor;
    Cursor &c = unit(location)->cursors[location];
    if (cursorPtr)
        *cursorPtr = &c;
    if (!c.isNull())
        return true;

    // if (mLogFile) {
    //     String out;
    //     Log(&out) << cursor << a;
    //     fwrite(out.constData(), 1, out.size(), mLogFile);
    //     fwrite("\n", 1, 1, mLogFile);
    // }
    CXStringScope name = clang_getCursorSpelling(cursor);
    const char *cstr = name.data();
    c.symbolLength = cstr ? strlen(cstr) : 0;
    c.type = clang_getCursorType(cursor).kind;
    c.location = location;
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
        default:
            unit(location)->cursors.remove(location);
            if (cursorPtr)
                *cursorPtr = 0;
            return false;
        }
    } else {
        String typeOverride;
        if (kind == CXCursor_VarDecl) {
            const CXCursor typeRef = resolveAutoTypeRef(cursor);
            if (!clang_equalCursors(typeRef, nullCursor)) {
                // const CXSourceRange range = clang_Cursor_getSpellingNameRange(mLastCursor, 0, 0);
                // error() << "Found" << typeRef << "for" << cursor << mLastCursor
                //         << createLocation(mLastCursor)
                //         << clang_Range_isNull(range)
                //         << createLocation(clang_getCursorLocation(mLastCursor));
                Cursor *cursorPtr = 0;
                if (handleReference(mLastCursor, CXCursor_TypeRef,
                                    createLocation(clang_getCursorLocation(mLastCursor)),
                                    clang_getCursorReferenced(typeRef), nullCursor, &cursorPtr)) {
                    // the type is read from the cursor passed in and that won't
                    // be correct in this case
                    cursorPtr->type = clang_getCursorType(typeRef).kind;
                    cursorPtr->symbolLength = 4;
                    typeOverride = cursorPtr->symbolName;
                    cursorPtr->symbolName += " (auto)";
                    cursorPtr->endLine = c.startLine;
                    cursorPtr->endColumn = c.startColumn + 4;
                }
            }
        }

        c.symbolName = addNamePermutations(cursor, location, typeOverride);
    }

    const CXSourceRange range = clang_getCursorExtent(cursor);
    const CXSourceLocation rangeStart = clang_getRangeStart(range);
    const CXSourceLocation rangeEnd = clang_getRangeEnd(range);
    unsigned startLine, startColumn, endLine, endColumn;
    clang_getSpellingLocation(rangeStart, 0, &startLine, &startColumn, 0);
    clang_getSpellingLocation(rangeEnd, 0, &endLine, &endColumn, 0);
    c.startLine = startLine;
    c.endLine = endLine;
    c.startColumn = startColumn;
    c.endColumn = endColumn;

    if (kind == CXCursor_EnumConstantDecl) {
#if CINDEX_VERSION_MINOR > 1
        c.enumValue = clang_getEnumConstantDeclValue(cursor);
#else
        c.definition = 1;
#endif
    } else {
        c.definition = clang_isCursorDefinition(cursor);
    }
    c.kind = kind;
    // apparently some function decls will give a different usr for
    // their definition and their declaration.  Using the canonical
    // cursor's usr allows us to join them. Check JSClassRelease in
    // JavaScriptCore for an example.
    c.usr = RTags::eatString(clang_getCursorUSR(clang_getCanonicalCursor(cursor)));
    if (!c.usr.isEmpty())
        unit(location)->usrs[c.usr].insert(location);

    switch (c.kind) {
    case CXCursor_CXXMethod: {
        List<Location> locations;
        locations.append(location);
        addOverriddenCursors(cursor, location, locations);
        c.parent = createLocation(clang_getCursorSemanticParent(cursor));
        break; }
    // fall through
    case CXCursor_Constructor:
    case CXCursor_Destructor:
        c.parent = createLocation(clang_getCursorSemanticParent(cursor));
        break;
    default:
        break;
    }

    return true;
}

bool ClangIndexer::parse()
{
    StopWatch sw;
    assert(!mClangUnit);
    assert(!mIndex);
    mIndex = clang_createIndex(0, 1);
    assert(mIndex);
    const unsigned int commandLineFlags = Source::FilterBlacklist|Source::IncludeDefines|Source::IncludeIncludepaths;
    const unsigned int flags = CXTranslationUnit_DetailedPreprocessingRecord;
    List<CXUnsavedFile> unsavedFiles(mUnsavedFiles.size() + 1);
    int unsavedIndex = 0;
    for (const auto &it : mUnsavedFiles) {
        unsavedFiles[unsavedIndex++] = {
            it.first.constData(),
            it.second.constData(),
            static_cast<unsigned long>(it.second.size())
        };
    }

    debug() << "CI::parse: " << mSource.toCommandLine(commandLineFlags) << "\n";

    // for (const auto it : mSource.toCommandLine(commandLineFlags)) {
    //     error("[%s]", it.constData());
    // }
    RTags::parseTranslationUnit(mSourceFile, mSource.toCommandLine(commandLineFlags), mClangUnit,
                                mIndex, &unsavedFiles[0], unsavedIndex, flags, &mClangLine);

    warning() << "CI::parse loading unit:" << mClangLine << " " << (mClangUnit != 0);
    if (mClangUnit) {
        clang_getInclusions(mClangUnit, ClangIndexer::inclusionVisitor, this);
        mParseDuration = sw.elapsed();
        return true;
    }
    error() << "Failed to parse" << mClangLine;
    for (Hash<uint32_t, bool>::const_iterator it = mData->visited.begin(); it != mData->visited.end(); ++it) {
        mData->dependencies[it->first].insert(mSource.fileId);
        if (it->second)
            addFileSymbol(it->first);
    }

    return false;
}

bool ClangIndexer::writeFiles(const Path &root, String &error)
{
    for (const auto &unit : mUnits) {
        String unitRoot = root;
        unitRoot << unit.first;
        Path::mkdir(unitRoot, Path::Recursive);
        if (!FileMap<Location, Cursor>::write(unitRoot + "/cursors", unit.second->cursors)) {
            error = "Failed to write cursors";
            return false;
        }
        if (!FileMap<Location, Map<Location, uint16_t> >::write(unitRoot + "/targets", unit.second->targets)) {
            error = "Failed to write targets";
            return false;
        }
        if (!FileMap<String, Set<Location> >::write(unitRoot + "/usrs", unit.second->usrs)) {
            error = "Failed to write usrs";
            return false;
        }
        if (!FileMap<String, Set<Location> >::write(unitRoot + "/symnames", unit.second->symbolNames)) {
            error = "Failed to write symbolNames";
            return false;
        }
    }
    String sourceRoot = root;
    sourceRoot << mSource.fileId;
    Path::mkdir(sourceRoot, Path::Recursive);
    sourceRoot << "/info";
    FILE *f = fopen(sourceRoot.constData(), "w");
    if (!f) {
        return false;
    }

    fprintf(f, "%s\n%s\n",
            mSourceFile.constData(),
            String::join(mSource.toCommandLine(Source::Default|Source::IncludeCompiler|Source::IncludeSourceFile), " ").constData());
    fclose(f);

    return true;
}

bool ClangIndexer::diagnose()
{
    if (!mClangUnit) {
        return false;
    }

    List<String> compilationErrors;
    const unsigned diagnosticCount = clang_getNumDiagnostics(mClangUnit);

    for (unsigned i=0; i<diagnosticCount; ++i) {
        CXDiagnostic diagnostic = clang_getDiagnostic(mClangUnit, i);
        const CXSourceLocation diagLoc = clang_getDiagnosticLocation(diagnostic);
        const Location loc = createLocation(diagLoc, 0);
        const uint32_t fileId = loc.fileId();
        if (mData->visited.value(fileId)) {
            const String msg = RTags::eatString(clang_getDiagnosticSpelling(diagnostic));
            const CXDiagnosticSeverity sev = clang_getDiagnosticSeverity(diagnostic);
            Diagnostic::Type type = Diagnostic::None;
            switch (sev) {
            case CXDiagnostic_Warning:
                type = Diagnostic::Warning;
                break;
            case CXDiagnostic_Error:
            case CXDiagnostic_Fatal:
                type = Diagnostic::Error;
                break;
            default:
                break;
            }
            if (type != Diagnostic::None) {
                const unsigned rangeCount = clang_getDiagnosticNumRanges(diagnostic);
                bool ok = false;
                for (unsigned rangePos = 0; rangePos < rangeCount; ++rangePos) {
                    const CXSourceRange range = clang_getDiagnosticRange(diagnostic, rangePos);
                    const CXSourceLocation start = clang_getRangeStart(range);
                    const CXSourceLocation end = clang_getRangeEnd(range);

                    unsigned startOffset, endOffset;
                    clang_getSpellingLocation(start, 0, 0, 0, &startOffset);
                    clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                    if (!rangePos && !startOffset && !endOffset) {
                        // huh, range invalid? fall back to diag location
                        break;
                    } else {
                        unsigned int line, column;
                        clang_getSpellingLocation(start, 0, &line, &column, 0);
                        const Location key(loc.fileId(), line, column);
                        mData->diagnostics[key] = Diagnostic(type, msg, endOffset - startOffset);
                        ok = true;
                        break;
                    }
                }
                if (!ok) {
                    unsigned line, column;
                    clang_getSpellingLocation(diagLoc, 0, &line, &column, 0);
                    const Location key(loc.fileId(), line, column);
                    mData->diagnostics[key] = Diagnostic(type, msg);
                    // no length
                }
            }
            // logDirect(RTags::CompilationError, msg.constData());

            const unsigned fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
            for (unsigned f=0; f<fixItCount; ++f) {
                CXSourceRange range;
                const CXStringScope stringScope = clang_getDiagnosticFixIt(diagnostic, f, &range);
                CXSourceLocation start = clang_getRangeStart(range);

                unsigned line, column;
                CXFile file;
                clang_getSpellingLocation(start, &file, &line, &column, 0);
                if (!file)
                    continue;
                CXStringScope fileName(clang_getFileName(file));

                const Location loc = createLocation(clang_getCString(fileName), line, column);
                if (mData->visited.value(loc.fileId())) {
                    unsigned int startOffset, endOffset;
                    CXSourceLocation end = clang_getRangeEnd(range);
                    clang_getSpellingLocation(start, 0, 0, 0, &startOffset);
                    clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                    const char *string = clang_getCString(stringScope);
                    error("Fixit for %s:%d:%d: Replace %d characters with [%s]", loc.path().constData(),
                          line, column, endOffset - startOffset, string);
                    Diagnostic &entry = mData->diagnostics[Location(loc.fileId(), line, column)];
                    entry.type = Diagnostic::Fixit;
                    if (entry.message.isEmpty()) {
                        entry.message = String::format<64>("did you mean '%s'?", string);
                    }
                    entry.length = endOffset - startOffset;
                    mData->fixIts[loc.fileId()].insert(FixIt(line, column, endOffset - startOffset, string));
                }
            }
        }

        clang_disposeDiagnostic(diagnostic);
    }

    for (Hash<uint32_t, bool>::const_iterator it = mData->visited.begin(); it != mData->visited.end(); ++it) {
        if (it->second) {
            const Location loc(it->first, 0, 0);
#if CINDEX_VERSION_MINOR >= 21
            CXFile file = clang_getFile(mClangUnit, loc.path().constData());
            if (file) {
                if (CXSourceRangeList *skipped = clang_getSkippedRanges(mClangUnit, file)) {
                    const unsigned count = skipped->count;
                    for (unsigned i=0; i<count; ++i) {
                        CXSourceLocation start = clang_getRangeStart(skipped->ranges[i]);

                        unsigned line, column, startOffset, endOffset;
                        clang_getSpellingLocation(start, 0, &line, &column, &startOffset);
                        Diagnostic &entry = mData->diagnostics[Location(loc.fileId(), line, column)];
                        if (entry.type == Diagnostic::None) {
                            CXSourceLocation end = clang_getRangeEnd(skipped->ranges[i]);
                            clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                            entry.type = Diagnostic::Skipped;
                            entry.length = endOffset - startOffset;
                            // error() << line << column << startOffset << endOffset;
                        }
                    }

                    clang_disposeSourceRangeList(skipped);
                    if (count)
                        continue;
                }
            }
#endif
            const Map<Location, Diagnostic>::const_iterator x = mData->diagnostics.lower_bound(loc);
            if (x == mData->diagnostics.end() || x->first.fileId() != it->first) {
                mData->diagnostics[loc] = Diagnostic();
            }
        }
    }

    return true;
}

bool ClangIndexer::visit()
{
    if (!mClangUnit || !mSource.fileId) {
        return false;
    }

    StopWatch watch;

    clang_visitChildren(clang_getTranslationUnitCursor(mClangUnit),
                        ClangIndexer::indexVisitor, this);

    for (Hash<uint32_t, bool>::const_iterator it = mData->visited.begin(); it != mData->visited.end(); ++it) {
        mData->dependencies[it->first].insert(mSource.fileId);
        addFileSymbol(it->first);
    }

    mVisitDuration = watch.elapsed();

    if (testLog(VerboseDebug)) {
        VerboseVisitorUserData u = { 0, "<VerboseVisitor " + mClangLine + ">\n", this };
        clang_visitChildren(clang_getTranslationUnitCursor(mClangUnit),
                            ClangIndexer::verboseVisitor, &u);
        u.out += "</VerboseVisitor " + mClangLine + ">";
        if (getenv("RTAGS_INDEXERJOB_DUMP_TO_FILE")) {
            char buf[1024];
            snprintf(buf, sizeof(buf), "/tmp/%s.log", Location::path(mSource.fileId).fileName());
            FILE *f = fopen(buf, "w");
            assert(f);
            fwrite(u.out.constData(), 1, u.out.size(), f);
            fclose(f);
        } else {
            logDirect(VerboseDebug, u.out);
        }
    }
    return true;
}

CXChildVisitResult ClangIndexer::verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
    Location loc = u->indexer->createLocation(cursor);
    if (loc.fileId()) {
        CXCursor ref = clang_getCursorReferenced(cursor);

        VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
        if (u->indent >= 0)
            u->out += String(u->indent, ' ');
        u->out += RTags::cursorToString(cursor);
        if (clang_equalCursors(ref, cursor)) {
            u->out += " refs self";
        } else if (!clang_equalCursors(ref, nullCursor)) {
            u->out += " refs " + RTags::cursorToString(ref);
        }

        if (loc.fileId() && u->indexer->mData->visited.value(loc.fileId())) {
            if (u->indexer->unit(loc)->cursors.contains(loc)) {
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
    unit(loc.fileId())->symbolNames[path].insert(loc);
    const char *fn = path.fileName();
    unit(loc.fileId())->symbolNames[fn].insert(loc);
}

void ClangIndexer::inclusionVisitor(CXFile includedFile,
                                    CXSourceLocation *includeStack,
                                    unsigned includeLen,
                                    CXClientData userData)
{
    ClangIndexer *indexer = static_cast<ClangIndexer*>(userData);
    const Location l = indexer->createLocation(includedFile, 1, 1);

    const uint32_t fileId = l.fileId();
    if (!includeLen) {
        indexer->mData->dependencies[fileId].insert(fileId);
    } else {
        for (unsigned i=0; i<includeLen; ++i) {
            CXFile originatingFile;
            clang_getSpellingLocation(includeStack[i], &originatingFile, 0, 0, 0);
            const Location loc = indexer->createLocation(originatingFile, 1, 1);
            const uint32_t f = loc.fileId();
            if (f)
                indexer->mData->dependencies[fileId].insert(f);
        }
    }
}

struct ResolveAutoTypeRefUserData
{
    CXCursor ref;
    int index;
    Set<String> *seen;
    // List<CXCursorKind> chain;
};

static CXChildVisitResult resolveAutoTypeRefVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    ResolveAutoTypeRefUserData *userData = reinterpret_cast<ResolveAutoTypeRefUserData*>(data);
    const CXCursorKind kind = clang_getCursorKind(cursor);
    const String usr = RTags::eatString(clang_getCursorUSR(cursor));
    if (!userData->seen->insert(usr)) {
        return CXChildVisit_Break;
    }
    // userData->chain.append(kind);
    // error() << "Got here" << cursor << userData->chain;
    switch (kind) {
    case CXCursor_TypeRef:
    case CXCursor_TemplateRef:
        // error() << "Found typeRef" << cursor;
        userData->ref = cursor;
        return CXChildVisit_Break;
    case CXCursor_DeclRefExpr:
    case CXCursor_UnexposedExpr: {
        CXCursor ref = clang_getCursorReferenced(cursor);
        // error() << "got unexposed expr ref" << ref;
        switch (clang_getCursorKind(ref)) {
        case CXCursor_VarDecl:
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod: {
            ResolveAutoTypeRefUserData u = { nullCursor, 0, userData->seen }; //, List<CXCursorKind>() };
            clang_visitChildren(ref, resolveAutoTypeRefVisitor, &u);
            // error() << "Visited for typeRef" << u.ref
            //         << clang_isInvalid(clang_getCursorKind(u.ref))
            //         << u.chain;
            if (!clang_equalCursors(u.ref, nullCursor)) {
                userData->ref = u.ref;
                return CXChildVisit_Break;
            }
            if (userData->index + u.index > 10)
                return CXChildVisit_Break;
            break; }
        default:
            break;
        }
        break; }
    case CXCursor_ParmDecl:
        // nothing to find here
        return CXChildVisit_Break;
    default:
        break;
    }
    return CXChildVisit_Recurse;
}

CXCursor ClangIndexer::resolveAutoTypeRef(const CXCursor &cursor) const
{
    assert(clang_getCursorKind(cursor) == CXCursor_VarDecl);
    Set<String> seen;
    ResolveAutoTypeRefUserData userData = { nullCursor, 0, &seen }; //, List<CXCursorKind>() };
    clang_visitChildren(cursor, resolveAutoTypeRefVisitor, &userData);
    if (userData.index > 1) {
        if (!clang_equalCursors(userData.ref, nullCursor)) {
            // error() << "Fixed cursor for" << cursor << userData.ref;
            // << userData.chain;
            return userData.ref;
            // } else {
            //     error() << "Couldn't fix cursor for" << cursor << userData.ref;
            //             // << userData.chain;
        }
    }
    // error() << "Need to find type for" << cursor << child;
    return nullCursor;
}

int ClangIndexer::symbolLength(CXCursorKind kind, const CXCursor &cursor) const
{
    if (kind == CXCursor_VarDecl) {
        const CXCursor typeRef = resolveAutoTypeRef(cursor);
        if (!clang_equalCursors(typeRef, nullCursor)) {
            return 4;
        }
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

Cursor ClangIndexer::findCursor(const Location &location, bool *ok) const
{
    Cursor ret;
    auto it = mUnits.find(location.fileId());
    if (it != mUnits.end()) {
        ret = it->second->cursors.value(location, Cursor(), ok);
    } else if (ok) {
        *ok = false;
    }
    return ret;
}
