#include "ClangIndexer.h"
#include "QueryMessage.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include <rct/Connection.h>
#include <rct/EventLoop.h>
#include "IndexerMessage.h"
#include "Cpp.h"

static const CXSourceLocation nullLocation = clang_getNullLocation();
static const CXCursor nullCursor = clang_getNullCursor();

struct DumpUserData {
    int indentLevel;
    ClangIndexer *indexer;
};

struct FindImplicitEqualsConstructorUserData {
    CXCursor &ref;
    bool &success;
};

struct VerboseVisitorUserData {
    int indent;
    String out;
    ClangIndexer *indexer;
};

ClangIndexer::ClangIndexer()
    : mUnit(0), mIndex(0), mLastCursor(nullCursor), mVisitFileResponseMessageFileId(0),
      mVisitFileResponseMessageVisit(0), mParseDuration(0), mVisitDuration(0),
      mCommunicationDuration(0), mBlocked(0), mAllowed(0), mIndexed(1), mVisitFileTimeout(0),
      mIndexerMessageTimeout(0), mFileIdsQueried(0), mId(0), mLogFile(0)
{
    mConnection.newMessage().connect(std::bind(&ClangIndexer::onMessage, this,
                                               std::placeholders::_1, std::placeholders::_2));
}

ClangIndexer::~ClangIndexer()
{
    if (mLogFile)
        fclose(mLogFile);
    if (mUnit)
        clang_disposeTranslationUnit(mUnit);
    if (mIndex)
        clang_disposeIndex(mIndex);
}

bool ClangIndexer::connect(const Path &serverFile)
{
    return mConnection.connectUnix(serverFile, 1000);
}

bool ClangIndexer::connect(const String &hostName, uint16_t port)
{
    return mConnection.connectTcp(hostName, port);
}

bool ClangIndexer::index(IndexerJob::IndexType type, const Source &source,
                         const std::shared_ptr<Cpp> &cpp, const Path &project)
{
    // FILE *f = fopen((String("/tmp/") + source.sourceFile().fileName()).constData(), "w");
    // fwrite(preprocessed.constData(), 1, preprocessed.size(), f);
    // fclose(f);

    // mLogFile = fopen(String::format("/tmp/%s", sourceFile.fileName()).constData(), "w");
    mData.reset(new IndexData(type));
    mData->key = source.key();
    mData->jobId = mId;
    mSource = source;
    mCpp = cpp;
    mProject = project;
    assert(mConnection.isConnected());
    mData->visited[source.fileId] = true;
    assert(type != IndexerJob::Invalid);
    parse() && visit() && diagnose();
    mData->parseTime = cpp->time;
    if (mData->type != IndexerJob::Dump) {
        mData->message = source.sourceFile().toTilde();
        if (!mUnit)
            mData->message += " error";
        mData->message += String::format<16>(" in %dms. ", mTimer.elapsed());
        if (mUnit) {
            const char *format = "(%d syms, %d symNames, %d refs, %d deps, %d of %d files, cursors: %d of %d, %d queried) (%d/%d/%dms)";
            mData->message += String::format<128>(format,
                                                  mData->symbols.size(), mData->symbolNames.size(), mData->references.size(),
                                                  mData->dependencies.size(), mIndexed, mData->visited.size(), mAllowed,
                                                  mAllowed + mBlocked, mFileIdsQueried, mParseDuration, mVisitDuration,
                                                  mCommunicationDuration);
        } else if (mData->dependencies.size()) {
            mData->message += String::format<16>("(%d deps)", mData->dependencies.size());
        }
        if (mData->type == IndexerJob::Dirty)
            mData->message += " (dirty)";
    }
    const IndexerMessage msg(mProject, mData);
    ++mFileIdsQueried;
    // FILE *f = fopen("/tmp/clangindex.log", "a");
    // fprintf(f, "Writing indexer message %d\n", mData->symbols.size());

    // if (Path::exists(String("/tmp/") + Location::path(mSource.fileId).fileName())) {
    //     error() << "Detected problem... crashing" << Location::path(mSource.fileId).fileName();
    //     Path::rm(String("/tmp/") + Location::path(mSource.fileId).fileName());
    //     sleep(1);
    //     abort();
    // }
    if (!mConnection.send(msg)) {
        error() << "Couldn't send IndexerMessage" << Location::path(mSource.fileId);
        return false;
    }
    mConnection.finished().connect(std::bind(&EventLoop::quit, EventLoop::eventLoop()));
    if (EventLoop::eventLoop()->exec(mIndexerMessageTimeout) == EventLoop::Timeout) {
        error() << "Couldn't send IndexerMessage (2)" << Location::path(mSource.fileId);
        return false;
    }
    // error() << "Must have gotten a finished" << Location::path(mSource.fileId);
    // fprintf(f, "Wrote indexer message %d\n", mData->symbols.size());
    // fclose(f);

    return true;
}

void ClangIndexer::onMessage(Message *msg, Connection *conn)
{
    assert(msg->messageId() == VisitFileResponseMessage::MessageId);
    const VisitFileResponseMessage *vm = static_cast<VisitFileResponseMessage*>(msg);
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
        resolved = sourceFile.resolved();
        id = Location::fileId(resolved);
        if (id)
            Location::set(sourceFile, id);
    }

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
                if (!mBlockedFiles.contains(sourceFile) && !mBlockedFiles.contains(resolved)) {
                    error() << "Something wrong" << sourceFile << resolved << id << mSource.sourceFile();
                }
                assert(mBlockedFiles.contains(sourceFile) || mBlockedFiles.contains(resolved));
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

    mVisitFileResponseMessageFileId = 0;
    mVisitFileResponseMessageVisit = false;
    mConnection.send(msg);
    StopWatch sw;
    EventLoop::eventLoop()->exec(mVisitFileTimeout);
    mCommunicationDuration += sw.elapsed();
    id = mVisitFileResponseMessageFileId;
    if (!id) {
        error() << "Error getting fileId for" << resolved;
        exit(1);
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

String ClangIndexer::addNamePermutations(const CXCursor &cursor, const Location &location)
{
    CXCursorKind kind = clang_getCursorKind(cursor);
    const CXCursorKind originalKind = kind;
    char buf[32768];
    int pos = sizeof(buf) - 1;
    buf[pos] = '\0';
    int cutoff = -1;

    CXCursor c = cursor;
    bool hasTemplates = false;
    do {
        CXStringScope displayName(clang_getCursorDisplayName(c));
        const char *name = displayName.data();
        if (!name)
            break;
        const int len = strlen(name);
        if (!len)
            break;

        if (kind == CXCursor_ClassTemplate)
            hasTemplates = true;
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

    String type;
    switch (originalKind) {
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_ClassTemplate:
        break;
    default:
        type = typeName(cursor);
        break;
    }
    // if (originalKind == CXCursor_ParmDecl)
    //     error() << "full:" << (buf + pos) << "type:" << type;
    if (cutoff == -1)
        cutoff = pos;
    String ret;
    // i == 0 --> with templates, i == 1 without templates or without EnumConstantDecl part
    for (int i=0; i<2; ++i) {
        {
            char *ch = buf + pos;
            while (true) {
                const String name(ch, sizeof(buf) - (ch - buf) - 1);
                mData->symbolNames[name].insert(location);
                if (!type.isEmpty() && (originalKind != CXCursor_ParmDecl || !strchr(ch, '('))) {
                    // We only want to add the type to the final declaration for ParmDecls
                    // e.g.
                    // void foo(int)::bar
                    // int bar
                    //
                    // not
                    // int void foo(int)::bar
                    // or
                    // void foo(int)::int bar

                    mData->symbolNames[type + name].insert(location);
                }

                ch = strstr(ch + 1, "::");

                if (ch) {
                    ch += 2;
                } else {
                    break;
                }
            }
        }
        if (i == 0) {
            // create actual symbol name that will go into CursorInfo. This doesn't include namespaces etc
            ret.assign(buf + cutoff, sizeof(buf) - cutoff - 1);
            if (!type.isEmpty())
                ret.prepend(type);
        }

        if (i == 1 || (!hasTemplates && originalKind != CXCursor_EnumConstantDecl)) {
            break;
        }

        if (originalKind == CXCursor_EnumConstantDecl) { // remove CXCursor_EnumDecl
            char *last = 0, *secondLast = 0;
            char *delimiter = buf + pos;
            while (true) {
                secondLast = last;
                last = delimiter;
                delimiter = strstr(delimiter, "::");
                if (delimiter) {
                    delimiter += 2;
                } else {
                    break;
                }
            }
            if (secondLast && last) {
                const int len = (last - secondLast);
                if (secondLast != buf + pos) {
                    memmove(buf + pos + len, buf + pos, secondLast - (buf + pos));
                }
                pos += len;
            }
        } else { // remove templates
            assert(hasTemplates);
            char *start = strchr(buf + pos, '<');
            assert(start);
            char *end = strchr(start, '>');
            const int templateSize = (end - start) + 1;
            assert(end);
            memmove(buf + pos + templateSize, buf + pos, start - (buf + pos));
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
    // error() << "indexVisitor" << cursor;
    // FILE *f = fopen("/tmp/clangindex.log", "a");
    // String str;
    // Log(&str) << cursor;
    // fwrite(str.constData(), 1, str.size(), f);
    // fwrite("\n", 1, 1, f);
    // fclose(f);
    ClangIndexer *indexer = static_cast<ClangIndexer*>(data);
    const LastCursorUpdater updater(indexer->mLastCursor, cursor);

    const CXCursorKind kind = clang_getCursorKind(cursor);
    const RTags::CursorType type = RTags::cursorType(kind);
    if (type == RTags::Other)
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
    case RTags::Cursor:
        indexer->handleCursor(cursor, kind, loc);
        break;
    case RTags::Include:
        indexer->handleInclude(cursor, kind, loc);
        break;
    case RTags::Reference:
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
    case RTags::Other:
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

void ClangIndexer::superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                            const Location &location, const CXCursor &ref,
                                                            const CXCursor &parent)
{
    // This is for references to superclass template functions. Awful awful
    // shit. See https://github.com/Andersbakken/rtags/issues/62 and commit
    // for details. I really should report this as a bug.
    if (kind == CXCursor_MemberRefExpr && clang_getCursorKind(parent) == CXCursor_CallExpr) {
        const CXCursor templateRef = RTags::findChild(cursor, CXCursor_TemplateRef);
        if (templateRef == CXCursor_TemplateRef) {
            const CXCursor classTemplate = clang_getCursorReferenced(templateRef);
            if (classTemplate == CXCursor_ClassTemplate) {
                FILE *f = fopen(location.path().constData(), "r");
                if (f) {
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
                            handleReference(cursor, kind, Location(location.fileId(), location.line(), location.column() + 1), alternatives.first(), parent);
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
                }
            }
        }
    }
}


void ClangIndexer::handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &location, const CXCursor &ref, const CXCursor &parent)
{
    const CXCursorKind refKind = clang_getCursorKind(ref);
    if (clang_isInvalid(refKind)) {
        superclassTemplateMemberFunctionUgleHack(cursor, kind, location, ref, parent);
        return;
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
        return;
    }

    switch (refKind) {
    case CXCursor_Constructor:
        if (isImplicit(ref))
            return;
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
                    return; // eat implicit operator calls
                isOperator = true;
            }
        }
        break; }
    default:
        break;
    }

    const Location reffedLoc = createLocation(ref);
    if (!reffedLoc.isValid())
        return;

    CursorInfo &refInfo = mData->symbols[reffedLoc];
    if (!refInfo.symbolLength && !handleCursor(ref, refKind, reffedLoc))
        return;

    refInfo.references.insert(location);

    CursorInfo &info = mData->symbols[location];
    info.targets.insert(reffedLoc);

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

    if (!RTags::isCursor(info.kind) && (!info.symbolLength || info.bestTarget(mData->symbols).kind == refKind)) {
        CXSourceRange range = clang_getCursorExtent(cursor);
        CXSourceLocation rangeStart = clang_getRangeStart(range);
        CXSourceLocation rangeEnd = clang_getRangeEnd(range);
        unsigned startLine, startColumn, endLine, endColumn;
        clang_getPresumedLocation(rangeStart, 0, &startLine, &startColumn);
        clang_getPresumedLocation(rangeEnd, 0, &endLine, &endColumn);
        info.startLine = startLine;
        info.startColumn = startColumn;
        info.endLine = endLine;
        info.endColumn = endColumn;
        info.definition = false;
        info.kind = kind;
        if (isOperator) {
            unsigned start, end;
            clang_getSpellingLocation(rangeStart, 0, 0, 0, &start);
            clang_getSpellingLocation(rangeEnd, 0, 0, 0, &end);
            info.symbolLength = end - start;
        } else {
            info.symbolLength = refInfo.symbolLength;
        }
        info.symbolName = refInfo.symbolName;
        info.type = clang_getCursorType(cursor).kind;
    }

    Set<Location> &val = mData->references[location];
    val.insert(reffedLoc);

}

void ClangIndexer::addOverriddenCursors(const CXCursor& cursor, const Location& location, List<CursorInfo*>& infos)
{
    CXCursor *overridden;
    unsigned count;
    clang_getOverriddenCursors(cursor, &overridden, &count);
    if (!overridden)
        return;
    for (unsigned i=0; i<count; ++i) {
        Location loc = createLocation(overridden[i]);
        CursorInfo &o = mData->symbols[loc];

        //error() << "adding overridden (1) " << location << " to " << o;
        o.references.insert(location);
        List<CursorInfo*>::const_iterator inf = infos.begin();
        const List<CursorInfo*>::const_iterator infend = infos.end();
        while (inf != infend) {
            //error() << "adding overridden (2) " << loc << " to " << *(*inf);
            (*inf)->references.insert(loc);
            ++inf;
        }

        infos.append(&o);
        addOverriddenCursors(overridden[i], loc, infos);
        infos.removeLast();
    }
    clang_disposeOverriddenCursors(overridden);
}

void ClangIndexer::handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location)
{
    assert(kind == CXCursor_InclusionDirective);
    (void)kind;
    CXFile includedFile = clang_getIncludedFile(cursor);
    if (includedFile) {
        const Location refLoc = createLocation(includedFile, 1, 0);
        if (!refLoc.isNull()) {
            {
                String include = "#include ";
                const Path path = refLoc.path();
                assert(mSource.fileId);
                mData->dependencies[refLoc.fileId()].insert(mSource.fileId);
                mData->symbolNames[(include + path)].insert(location);
                mData->symbolNames[(include + path.fileName())].insert(location);
            }
            CursorInfo &info = mData->symbols[location];
            info.targets.insert(refLoc);
            info.kind = cursor.kind;
            info.definition = false;
            info.symbolName = "#include " + RTags::eatString(clang_getCursorDisplayName(cursor));
            info.symbolLength = info.symbolName.size() + 2;
            // this fails for things like:
            // # include    <foobar.h>
        }
    }
}

static inline bool isInline(const CXCursor &cursor)
{
    switch (clang_getCursorKind(clang_getCursorLexicalParent(cursor))) {
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
    case CXCursor_StructDecl:
        return true;
    default:
        return false;
    }
}

const char *builtinTypeName(CXTypeKind kind)
{
    const char *ret = 0;
    switch (kind) {
    case CXType_Void: ret ="void"; break;
    case CXType_Bool: ret ="bool"; break;
    case CXType_Char_U: ret ="unsigned char"; break;
    case CXType_UChar: ret ="unsigned char"; break;
    case CXType_Char16: ret ="char16"; break;
    case CXType_Char32: ret ="char32"; break;
    case CXType_UShort: ret ="unsigned short"; break;
    case CXType_UInt: ret ="unsigned int"; break;
    case CXType_ULong: ret ="unsigned long"; break;
    case CXType_ULongLong: ret ="unsigned long long"; break;
    case CXType_UInt128: ret ="uint128"; break;
    case CXType_Char_S: ret ="char"; break;
    case CXType_SChar: ret ="schar"; break;
    case CXType_WChar: ret ="wchar"; break;
    case CXType_Short: ret ="short"; break;
    case CXType_Int: ret ="int"; break;
    case CXType_Long: ret ="long"; break;
    case CXType_LongLong: ret ="long long"; break;
    case CXType_Int128: ret ="int128"; break;
    case CXType_Float: ret ="float"; break;
    case CXType_Double: ret ="double"; break;
    case CXType_LongDouble: ret ="long double"; break;
    default:
        break;
    }
    return ret;
}

inline String ClangIndexer::typeString(const CXType &type)
{
    String ret;
    if (clang_isConstQualifiedType(type))
        ret = "const ";

    const char *builtIn = builtinTypeName(type.kind);
    if (builtIn) {
        ret += builtIn;
        return ret;
    }

    if (char pointer = (type.kind == CXType_Pointer ? '*' : (type.kind == CXType_LValueReference ? '&' : 0))) {
        const CXType pointee = clang_getPointeeType(type);
        ret += typeString(pointee);
        if (ret.endsWith('*') || ret.endsWith('&')) {
            ret += pointer;
        } else {
            ret += ' ';
            ret += pointer;
        }
        return ret;
    }

    if (type.kind == CXType_ConstantArray) {
        ret += typeString(clang_getArrayElementType(type));
#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR >= 1)
        const long long count = clang_getNumElements(type);
        ret += '[';
        if (count >= 0)
            ret += String::number(count);
        ret += ']';
#endif
        return ret;
    }
    ret += ClangIndexer::typeName(clang_getTypeDeclaration(type));
    if (ret.endsWith(' '))
        ret.chop(1);
    return ret;
}

String ClangIndexer::typeName(const CXCursor &cursor)
{
    String ret;
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_FunctionTemplate:
        // ### If the return value is a template type we get an empty string here
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
        ret = typeString(clang_getResultType(clang_getCursorType(cursor)));
        break;
    case CXCursor_ClassTemplate:
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_UnionDecl:
    case CXCursor_TypedefDecl:
    case CXCursor_EnumDecl:
        ret = RTags::eatString(clang_getCursorSpelling(cursor));
        break;
    case CXCursor_VarDecl: {
        const CXCursor initType = RTags::findFirstChild(cursor);
        if (clang_getCursorKind(initType) == CXCursor_InitListExpr) {
            ret = typeString(clang_getCursorType(initType));
        } else {
            ret = typeString(clang_getCursorType(cursor));
        }
        break; }
    case CXCursor_FieldDecl: // ### If the return value is a template type we get an empty string here
    case CXCursor_ParmDecl:
        ret = typeString(clang_getCursorType(cursor));
        break;
    default:
        return String();
    }
    if (!ret.isEmpty() && !ret.endsWith('*') && !ret.endsWith('&'))
        ret.append(' ');
    return ret;
}

bool ClangIndexer::handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location)
{
    // error() << "Got a cursor" << cursor;
    CursorInfo &info = mData->symbols[location];
    if (!info.symbolLength || !RTags::isCursor(info.kind)) {
        // if (mLogFile) {
        //     String out;
        //     Log(&out) << cursor << a;
        //     fwrite(out.constData(), 1, out.size(), mLogFile);
        //     fwrite("\n", 1, 1, mLogFile);
        // }
        CXStringScope name = clang_getCursorSpelling(cursor);
        const char *cstr = name.data();
        info.symbolLength = cstr ? strlen(cstr) : 0;
        info.type = clang_getCursorType(cursor).kind;
        if (!info.symbolLength) {
            // this is for these constructs:
            // typedef struct {
            //    int a;
            // } foobar;
            //
            // We end up not getting a spelling for the cursor

            switch (kind) {
            case CXCursor_ClassDecl:
                info.symbolLength = 5;
                info.symbolName = "class";
                break;
            case CXCursor_UnionDecl:
                info.symbolLength = 5;
                info.symbolName = "union";
                break;
            case CXCursor_StructDecl:
                info.symbolLength = 6;
                info.symbolName = "struct";
                break;
            default:
                mData->symbols.remove(location);
                return false;
            }
        } else {
            info.symbolName = addNamePermutations(cursor, location);
        }

        CXSourceRange range = clang_getCursorExtent(cursor);
        CXSourceLocation rangeStart = clang_getRangeStart(range);
        CXSourceLocation rangeEnd = clang_getRangeEnd(range);
        unsigned startLine, startColumn, endLine, endColumn;
        clang_getPresumedLocation(rangeStart, 0, &startLine, &startColumn);
        clang_getPresumedLocation(rangeEnd, 0, &endLine, &endColumn);
        info.startLine = startLine;
        info.startColumn = startColumn;
        info.endLine = endLine;
        info.endColumn = endColumn;

        if (kind == CXCursor_EnumConstantDecl) {
#if CINDEX_VERSION_MINOR > 1
            info.enumValue = clang_getEnumConstantDeclValue(cursor);
#else
            info.definition = 1;
#endif
        } else {
            info.definition = clang_isCursorDefinition(cursor);
        }
        info.kind = kind;
        // apparently some function decls will give a different usr for
        // their definition and their declaration.  Using the canonical
        // cursor's usr allows us to join them. Check JSClassRelease in
        // JavaScriptCore for an example.
        const String usr = RTags::eatString(clang_getCursorUSR(clang_getCanonicalCursor(cursor)));
        if (!usr.isEmpty())
            mData->usrMap[usr].insert(location);

        switch (info.kind) {
        case CXCursor_Constructor:
        case CXCursor_Destructor: {
            Location parentLocation = createLocation(clang_getCursorSemanticParent(cursor));
            // consider doing this for only declaration/inline definition since
            // declaration and definition should know of one another
            if (parentLocation.isValid()) {
                CursorInfo &parent = mData->symbols[parentLocation];
                parent.references.insert(location);
                info.references.insert(parentLocation);
            }
            break; }
        case CXCursor_CXXMethod: {
            List<CursorInfo*> infos;
            infos.append(&info);
            addOverriddenCursors(cursor, location, infos);
            break; }
        default:
            break;
        }
    }

    return true;
}

bool ClangIndexer::parse()
{
    StopWatch sw;
    assert(!mUnit);
    assert(!mIndex);
    mIndex = clang_createIndex(0, 0);
    assert(mIndex);
    const Path sourceFile = Location::path(mSource.fileId);
    // error() << "mContents" << mContents.size();
    CXUnsavedFile unsaved = {
        sourceFile.constData(),
        mCpp->preprocessed.constData(),
        static_cast<unsigned long>(mCpp->preprocessed.size())
    };
    RTags::parseTranslationUnit(sourceFile, mSource.arguments, List<String>(), mUnit, mIndex, mClangLine, &unsaved, 1);

    mData->parseTime = mTimer.elapsed();
    warning() << "loading mUnit " << mClangLine << " " << (mUnit != 0);
    if (mUnit) {
        mParseDuration = sw.elapsed();
        return true;
    }
    error() << "got failure" << mClangLine;
    const String preprocessorOnly = RTags::filterPreprocessor(sourceFile);
    if (!preprocessorOnly.isEmpty()) {
        CXUnsavedFile preprocessorOnlyUnsaved = {
            sourceFile.constData(), preprocessorOnly.constData(),
            static_cast<unsigned long>(preprocessorOnly.size())
        };
        RTags::parseTranslationUnit(sourceFile, mSource.arguments, List<String>(),
                                    mUnit, mIndex, mClangLine,
                                    &preprocessorOnlyUnsaved, 1);
    }
    if (mUnit) {
        clang_getInclusions(mUnit, ClangIndexer::inclusionVisitor, this);
        clang_disposeTranslationUnit(mUnit);
        mUnit = 0;
    } else if (mData->type != IndexerJob::Dump) {
        mData->dependencies[mSource.fileId].insert(mSource.fileId);
    }
    mParseDuration = sw.elapsed();
    return false;
}

struct XmlEntry
{
    enum Type { None, Warning, Error, Fixit };

    XmlEntry(Type t = None, const String &m = String(), int l = -1)
        : type(t), message(m), length(-1)
    {
    }

    Type type;
    String message;
    int length;
};

static inline String xmlEscape(const String& xml)
{
    if (xml.isEmpty())
        return xml;

    std::ostringstream strm;
    const char* ch = xml.constData();
    bool done = false;
    for (;;) {
        switch (*ch) {
        case '\0':
            done = true;
            break;
        case '"':
            strm << "\\\"";
            break;
        case '<':
            strm << "&lt;";
            break;
        case '>':
            strm << "&gt;";
            break;
        case '&':
            strm << "&amp;";
            break;
        default:
            strm << *ch;
            break;
        }
        if (done)
            break;
        ++ch;
    }
    return strm.str();
}

bool ClangIndexer::diagnose()
{
    if (!mUnit) {
        return false;
    } else if (mData->type == IndexerJob::Dump) {
        return true;
    }

    List<String> compilationErrors;
    const unsigned diagnosticCount = clang_getNumDiagnostics(mUnit);

    Map<Location, XmlEntry> xmlEntries;

    for (unsigned i=0; i<diagnosticCount; ++i) {
        CXDiagnostic diagnostic = clang_getDiagnostic(mUnit, i);
        const CXSourceLocation diagLoc = clang_getDiagnosticLocation(diagnostic);
        const Location loc = createLocation(diagLoc, 0);
        const uint32_t fileId = loc.fileId();
        if (mData->visited.value(fileId)) {
            const String msg = RTags::eatString(clang_getDiagnosticSpelling(diagnostic));
            const CXDiagnosticSeverity sev = clang_getDiagnosticSeverity(diagnostic);
            XmlEntry::Type type = XmlEntry::None;
            switch (sev) {
            case CXDiagnostic_Warning:
                type = XmlEntry::Warning;
                break;
            case CXDiagnostic_Error:
            case CXDiagnostic_Fatal:
                type = XmlEntry::Error;
                break;
            default:
                break;
            }
            if (type != XmlEntry::None) {
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
                        clang_getPresumedLocation(start, 0, &line, &column);
                        const Location key(loc.fileId(), line, column);
                        xmlEntries[key] = XmlEntry(type, msg, endOffset - startOffset);
                        ok = true;
                        break;
                    }
                }
                if (!ok) {
                    unsigned line, column;
                    clang_getPresumedLocation(diagLoc, 0, &line, &column);
                    const Location key(loc.fileId(), line, column);
                    xmlEntries[key] = XmlEntry(type, msg);
                    // no length
                }
            }
            // logDirect(RTags::CompilationError, msg.constData());

            const unsigned fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
            for (unsigned f=0; f<fixItCount; ++f) {
                unsigned line, column;
                CXString file;
                CXStringScope fileScope(file);
                CXSourceRange range;
                const CXStringScope stringScope = clang_getDiagnosticFixIt(diagnostic, f, &range);
                CXSourceLocation start = clang_getRangeStart(range);
                clang_getPresumedLocation(start, &file, &line, &column);

                const Location loc = createLocation(clang_getCString(file), line, column);
                if (mData->visited.value(loc.fileId())) {
                    unsigned int startOffset, endOffset;
                    CXSourceLocation end = clang_getRangeEnd(range);
                    clang_getSpellingLocation(start, 0, 0, 0, &startOffset);
                    clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                    const char *string = clang_getCString(stringScope);
                    error("Fixit for %s:%d:%d: Replace %d chars with [%s]", loc.path().constData(),
                          line, column, endOffset - startOffset, string);
                    XmlEntry &entry = xmlEntries[Location(loc.fileId(), line, column)];
                    entry.type = XmlEntry::Fixit;
                    if (entry.message.isEmpty()) {
                        entry.message = String::format<64>("did you mean '%s'?", string);
                    }
                    entry.length = endOffset - startOffset;
                    // if (testLog(logLevel) || testLog(RTags::CompilationError)) {
                    //     const String msg = String::format<128>("Fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
                    //                                            startOffset, endOffset, string);
                    //     if (testLog(logLevel))
                    //         logDirect(logLevel, msg.constData());
                    //     if (testLog(RTags::CompilationError))
                    //         logDirect(RTags::CompilationError, msg.constData());
                    // }
                    mData->fixIts[loc.fileId()].insert(FixIt(line, column, endOffset - startOffset, string));
                }
            }
        }

        clang_disposeDiagnostic(diagnostic);
    }

    for (List<Cpp::Diagnostic>::const_iterator it = mCpp->diagnostics.begin(); it != mCpp->diagnostics.end(); ++it) {
        XmlEntry::Type type = XmlEntry::None;
        switch (it->type) {
        case Cpp::Diagnostic::Note:
            break;
        case Cpp::Diagnostic::Warning:
            type = XmlEntry::Warning;
            break;
        case Cpp::Diagnostic::Error:
            type = XmlEntry::Error;
            break;
        }
        if (type != XmlEntry::None)
            xmlEntries[it->location] = XmlEntry(type, it->text);
    }

    mData->xmlDiagnostics = "<?xml version=\"1.0\" encoding=\"utf-8\"?><checkstyle>";
    if (!xmlEntries.isEmpty()) {
        Map<Location, XmlEntry>::const_iterator entry = xmlEntries.begin();
        const Map<Location, XmlEntry>::const_iterator end = xmlEntries.end();

        const char *severities[] = { "none", "warning", "error", "fixit" };

        uint32_t lastFileId = 0;
        while (entry != end) {
            const Location &loc = entry->first;
            const XmlEntry &xmlEntry = entry->second;
            if (loc.fileId() != lastFileId) {
                if (lastFileId)
                    mData->xmlDiagnostics += "</file>";
                lastFileId = loc.fileId();
                mData->xmlDiagnostics += String::format<128>("<file name=\"%s\">", loc.path().constData());
            }
            mData->xmlDiagnostics += String::format("<error line=\"%d\" column=\"%d\" %sseverity=\"%s\" message=\"%s\"/>",
                                                    loc.line(), loc.column(),
                                                    (xmlEntry.length <= 0 ? ""
                                                     : String::format<32>("length=\"%d\" ", xmlEntry.length).constData()),
                                                    severities[xmlEntry.type], xmlEscape(xmlEntry.message).constData());
            ++entry;
        }
        if (lastFileId)
            mData->xmlDiagnostics += "</file>";
    }

    for (Hash<uint32_t, bool>::const_iterator it = mData->visited.begin(); it != mData->visited.end(); ++it) {
        if (it->second) {
            const Map<Location, XmlEntry>::const_iterator x = xmlEntries.find(Location(it->first, 0, 0));
            if (x == xmlEntries.end() || x->first.fileId() != it->first) {
                const String fn = Location::path(it->first);
                mData->xmlDiagnostics += String::format("<file name=\"%s\"/>", fn.constData());
            }
        }
    }

    mData->xmlDiagnostics += "</checkstyle>";
    return true;
}

bool ClangIndexer::visit()
{
    if (!mUnit || !mSource.fileId) {
        return false;
    }

    if (mData->type == IndexerJob::Dump) {
        DumpUserData userData = { 0, this };
        clang_visitChildren(clang_getTranslationUnitCursor(mUnit),
                            ClangIndexer::dumpVisitor, &userData);
        return true;
    }
    StopWatch watch;

    clang_visitChildren(clang_getTranslationUnitCursor(mUnit),
                        ClangIndexer::indexVisitor, this);

    for (Hash<uint32_t, bool>::const_iterator it = mData->visited.begin(); it != mData->visited.end(); ++it) {
        mData->dependencies[it->first].insert(mSource.fileId);
        addFileSymbol(it->first);
    }

    mVisitDuration = watch.elapsed();

    if (testLog(VerboseDebug)) {
        VerboseVisitorUserData u = { 0, "<VerboseVisitor " + mClangLine + ">\n", this };
        clang_visitChildren(clang_getTranslationUnitCursor(mUnit),
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
            if (u->indexer->mData->references.contains(loc)) {
                u->out += " used as reference\n";
            } else if (u->indexer->mData->symbols.contains(loc)) {
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

CXChildVisitResult ClangIndexer::dumpVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    DumpUserData *dump = reinterpret_cast<DumpUserData*>(userData);
    assert(dump);
    assert(dump->indexer);
    Location loc = dump->indexer->createLocation(cursor);
    if (loc.fileId()) {
        CXCursor ref = clang_getCursorReferenced(cursor);
        dump->indexer->mData->message.append(loc.context());
        dump->indexer->mData->message.append(String::format<32>(" // %d, %d: ", loc.column(), dump->indentLevel));
        dump->indexer->mData->message.append(RTags::cursorToString(cursor, RTags::AllCursorToStringFlags));
        dump->indexer->mData->message.append(" " + typeName(cursor) + " ");
        if (clang_equalCursors(ref, cursor)) {
            dump->indexer->mData->message.append("refs self");
        } else if (!clang_equalCursors(ref, nullCursor)) {
            dump->indexer->mData->message.append("refs ");
            dump->indexer->mData->message.append(RTags::cursorToString(ref, RTags::AllCursorToStringFlags));
        }
        dump->indexer->mData->message += '\n';
    }
    ++dump->indentLevel;
    clang_visitChildren(cursor, ClangIndexer::dumpVisitor, userData);
    --dump->indentLevel;
    return CXChildVisit_Continue;
}

void ClangIndexer::addFileSymbol(uint32_t file)
{
    const Location loc(file, 1, 0);
    const Path path = Location::path(file);
    mData->symbolNames[path].insert(loc);
    const char *fn = path.fileName();
    mData->symbolNames[String(fn, strlen(fn))].insert(loc);
}


void ClangIndexer::inclusionVisitor(CXFile includedFile,
                                    CXSourceLocation *includeStack,
                                    unsigned includeLen,
                                    CXClientData userData)
{
    ClangIndexer *indexer = static_cast<ClangIndexer*>(userData);
    const Location l = indexer->createLocation(includedFile, 1, 0);

    const uint32_t fileId = l.fileId();
    if (!includeLen) {
        indexer->mData->dependencies[fileId].insert(fileId);
    } else {
        for (unsigned i=0; i<includeLen; ++i) {
            CXFile originatingFile;
            clang_getSpellingLocation(includeStack[i], &originatingFile, 0, 0, 0);
            const Location loc = indexer->createLocation(originatingFile, 1, 0);
            const uint32_t f = loc.fileId();
            if (f)
                indexer->mData->dependencies[fileId].insert(f);
        }
    }
}

void ClangIndexer::setBlockedFiles(Hash<Path, uint32_t> &&blockedFiles)
{
    mBlockedFiles = std::forward<Hash<Path, uint32_t> >(blockedFiles);
}
