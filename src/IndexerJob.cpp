#include "IndexerJob.h"
#include "Timer.h"
#include "MemoryMonitor.h"
#include "Server.h"
#include "EventLoop.h"
#include "FileInformation.h"
#include "Database.h"

static inline int writeSymbolNames(SymbolNameMap &symbolNames, ScopedDB db)
{
    Batch batch(db);
    int totalWritten = 0;

    SymbolNameMap::iterator it = symbolNames.begin();
    const SymbolNameMap::const_iterator end = symbolNames.end();
    while (it != end) {
        const char *key = it->first.constData();
        const Set<Location> added = it->second;
        bool ok;
        Set<Location> current = db->value<Set<Location> >(key, &ok);
        if (!ok) {
            totalWritten += batch.add(key, added);
        } else if (RTags::addTo(current, added)) {
            totalWritten += batch.add(key, current);
        }
        ++it;
    }

    return totalWritten;
}


static inline int writeSymbols(SymbolMap &symbols, const ReferenceMap &references, ScopedDB db)
{
    Batch batch(db);
    int totalWritten = 0;

    if (!references.isEmpty()) {
        const ReferenceMap::const_iterator end = references.end();
        for (ReferenceMap::const_iterator it = references.begin(); it != end; ++it) {
            CursorInfo &ci = symbols[it->second.first];
            if (it->second.second != RTags::NormalReference) {
                CursorInfo &other = symbols[it->first];
                // error() << "trying to join" << it->first << "and" << it->second.front();
                if (other.target.isNull())
                    other.target = it->second.first;
                if (ci.target.isNull())
                    ci.target = it->first;
            } else {
                ci.references.insert(it->first);
            }
        }
    }
    if (!symbols.isEmpty()) {
        SymbolMap::iterator it = symbols.begin();
        const SymbolMap::const_iterator end = symbols.end();
        while (it != end) {
            char buf[8];
            it->first.toKey(buf);
            const Slice key(buf, 8);
            CursorInfo added = it->second;
            bool ok;
            CursorInfo current = db->value<CursorInfo>(key, &ok);
            if (!ok) {
                totalWritten += batch.add(key, added);
            } else if (current.unite(added)) {
                totalWritten += batch.add(key, current);
            }
            ++it;
        }
    }
    return totalWritten;
}

static ByteArray pchFileName(const Path &pchDir, const Path &header)
{
    Path ret = header;
    RTags::encodePath(ret);
    ret.prepend(pchDir + "/pch/");
    return ret;
}

static inline Map<Path, Path> extractPchFiles(const Path &pchDir, const List<ByteArray> &args)
{
    Map<Path, Path> out;
    bool nextIsPch = false;
    const int count = args.size();
    for (int i=0; i<count; ++i) {
        const ByteArray &arg = args.at(i);
        if (arg.isEmpty())
            continue;

        if (nextIsPch) {
            nextIsPch = false;
            const Path header = arg;
            const Path pchFile = pchFileName(pchDir, header);
            out[header] = pchFile;
        } else if (arg == "-include-pch") {
            nextIsPch = true;
        }
    }
    return out;
}

static inline int writeFileInformation(uint32_t fileId, const List<ByteArray> &args,
                                       time_t lastTouched, ScopedDB db)
{
    if (Location::path(fileId).isHeader() && !RTags::isPch(args)) {
        error() << "Somehow we're writing fileInformation for a header that isn't pch"
                << Location::path(fileId) << args << lastTouched;
    }
    const char *ch = reinterpret_cast<const char*>(&fileId);
    return db->setValue(Slice(ch, sizeof(fileId)), FileInformation(lastTouched, args));
}


IndexerJob::IndexerJob(Indexer *indexer, unsigned flags,
                       const Path &input, const List<ByteArray> &arguments)

    : mFlags(flags), mIsPch(false), mDoneFullUSRScan(false), mIn(input),
      mFileId(Location::insertFile(input)), mArgs(arguments), mIndexer(indexer),
      mPchHeaders(extractPchFiles(mIndexer->projectRoot(), arguments)), mUnit(0)
{
}

static inline uint32_t fileId(CXFile file)
{
    return Location(file, 0).fileId();
}

void IndexerJob::inclusionVisitor(CXFile includedFile,
                                  CXSourceLocation *includeStack,
                                  unsigned includeLen,
                                  CXClientData userData)
{
    IndexerJob *job = static_cast<IndexerJob*>(userData);
    if (job->isAborted())
        return;
    const Location l(includedFile, 0);

    const Path path = l.path();
    job->mSymbolNames[path].insert(l);
    const char *fn = path.fileName();
    job->mSymbolNames[ByteArray(fn, strlen(fn))].insert(l);

    const uint32_t fileId = l.fileId();
    if (!includeLen) {
        job->mDependencies[fileId].insert(fileId);
        if (job->mIsPch)
            job->mPchDependencies.insert(fileId);
    } else {
        for (unsigned i=0; i<includeLen; ++i) {
            CXFile originatingFile;
            clang_getSpellingLocation(includeStack[i], &originatingFile, 0, 0, 0);
            Location loc(originatingFile, 0);
            const uint32_t f = loc.fileId();
            if (f)
                job->mDependencies[fileId].insert(f);
        }
        if (job->mIsPch) {
            job->mPchDependencies.insert(fileId);
        }
    }
}

static inline bool mayHaveTemplates(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_ClassTemplate:
    case CXCursor_Constructor:
    case CXCursor_Destructor:
        return true;
    default:
        return false;
    }
}

static inline void addToSymbolNames(const ByteArray &arg, bool hasTemplates, const Location &location, SymbolNameMap &symbolNames)
{
    symbolNames[arg].insert(location);
    if (hasTemplates) {
        ByteArray copy = arg;
        const int lt = arg.indexOf('<');
        if (lt == -1)
            return;
        const int gt = arg.indexOf('>', lt + 1);
        if (gt == -1)
            return;
        if (gt + 1 == arg.size()) {
            copy.truncate(lt);
        } else {
            copy.remove(lt, gt - lt + 1);
        }

        symbolNames[copy].insert(location);
    }
}

static const CXCursor nullCursor = clang_getNullCursor();
ByteArray IndexerJob::addNamePermutations(const CXCursor &cursor, const Location &location, bool addToDB)
{
    ByteArray ret, qname, qparam, qnoparam;

    CXCursor cur = cursor;
    CXCursorKind kind;
    bool first = true;
    for (;;) {
        if (clang_equalCursors(cur, nullCursor))
            break;
        kind = clang_getCursorKind(cur);
        if (!first) {
            bool ok = false;
            switch (kind) {
            case CXCursor_Namespace:
            case CXCursor_ClassDecl:
            case CXCursor_ClassTemplate:
            case CXCursor_StructDecl:
            case CXCursor_CXXMethod:
            case CXCursor_Constructor:
            case CXCursor_Destructor:
            case CXCursor_FunctionDecl:
                ok = true;
                break;
            default:
                break;
            }
            if (!ok)
                break;
        }

        CXStringScope displayName(clang_getCursorDisplayName(cur));
        const char *name = clang_getCString(displayName.string);
        if (!name || !strlen(name)) {
            break;
        }
        qname = ByteArray(name);
        if (ret.isEmpty()) {
            ret = qname;
            if (!addToDB)
                return ret;
        }
        if (qparam.isEmpty()) {
            qparam = qname;
            const int sp = qparam.indexOf('(');
            if (sp != -1)
                qnoparam = qparam.left(sp);
        } else {
            qparam.prepend(qname + "::");
            if (!qnoparam.isEmpty())
                qnoparam.prepend(qname + "::");
        }

        assert(!qparam.isEmpty());
        const bool hasTemplates = mayHaveTemplates(kind) && qnoparam.contains('<');
        addToSymbolNames(qparam, hasTemplates, location, mSymbolNames);
        if (!qnoparam.isEmpty()) {
            assert(!qnoparam.isEmpty());
            addToSymbolNames(qnoparam, hasTemplates, location, mSymbolNames);
        }

        if (first) {
            first = false;
            switch (kind) {
            case CXCursor_Namespace:
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
            case CXCursor_CXXMethod:
            case CXCursor_Constructor:
            case CXCursor_FunctionDecl:
            case CXCursor_Destructor:
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
            case CXCursor_FieldDecl:
            case CXCursor_ClassTemplate:
                break;
            default:
                // these don't need the scope
                return ret;
            }
        }

        cur = clang_getCursorSemanticParent(cur);
    }
    return ret;
}

static const CXSourceLocation nullLocation = clang_getNullLocation();
Location IndexerJob::createLocation(const CXCursor &cursor)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    if (!clang_equalLocations(location, nullLocation)) {
        CXFile file;
        unsigned start;
        clang_getSpellingLocation(location, &file, 0, 0, &start);
        if (file) {
            return Location(file, start);
        }
    }
    return Location();
}

Location IndexerJob::createLocation(const CXCursor &cursor, bool *blocked)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    Location ret;
    if (!clang_equalLocations(location, nullLocation)) {
        CXFile file;
        unsigned start;
        clang_getSpellingLocation(location, &file, 0, 0, &start);
        if (file) {
            ByteArray fileName = RTags::eatString(clang_getFileName(file));
            uint32_t &fileId = mFileIds[fileName];
            if (!fileId)
                fileId = Location::insertFile(Path::resolved(fileName));
            ret = Location(fileId, start);
            if (blocked) {
                PathState &state = mPaths[fileId];
                if (state == Unset) {
                    state = mIndexer->visitFile(fileId, mIn, mIsPch) ? Index : DontIndex;
                }
                if (state != Index) {
                    *blocked = true;
                    return Location();
                }
                *blocked = false;
            } else {
                PathState &state = mPaths[fileId];
                if (state == Unset || state == DontIndex) {
                    state = Reference;
                }
            }
        }
    }
    return ret;
}

static inline bool isInteresting(CXCursorKind kind)
{
    if (clang_isInvalid(kind))
        return false;
    switch (kind) {
    case CXCursor_AsmStmt:
    case CXCursor_CXXThisExpr:
    case CXCursor_CXXTypeidExpr:
    case CXCursor_CXXStaticCastExpr:
    case CXCursor_CXXNullPtrLiteralExpr:
    case CXCursor_CXXNewExpr: // ### Are these right?
    case CXCursor_CXXDeleteExpr:
    case CXCursor_CompoundAssignOperator: // ### Are these right?
    case CXCursor_CompoundStmt:
    case CXCursor_ParenExpr:
    case CXCursor_StringLiteral:
    case CXCursor_IntegerLiteral:
    case CXCursor_InitListExpr:
    case CXCursor_BreakStmt:
    case CXCursor_DefaultStmt:
    case CXCursor_BinaryOperator:
    case CXCursor_CaseStmt:
    case CXCursor_ConditionalOperator:
    case CXCursor_CStyleCastExpr:
    case CXCursor_ForStmt:
    case CXCursor_WhileStmt:
    case CXCursor_DoStmt:
    case CXCursor_IfStmt:
    case CXCursor_SwitchStmt:
    case CXCursor_CXXTryStmt:
    case CXCursor_CXXCatchStmt:
    case CXCursor_ContinueStmt:
    case CXCursor_CXXThrowExpr:
    case CXCursor_NullStmt:
    case CXCursor_ArraySubscriptExpr:
    case CXCursor_CXXBoolLiteralExpr:
    case CXCursor_CharacterLiteral:
    case CXCursor_UnaryOperator:
    case CXCursor_ReturnStmt:
    case CXCursor_CXXAccessSpecifier:
    case CXCursor_CXXConstCastExpr:
    case CXCursor_CXXDynamicCastExpr:
    case CXCursor_CXXReinterpretCastExpr:
    case CXCursor_TemplateTypeParameter:
    case CXCursor_NonTypeTemplateParameter:
        return false;
    default:
        break;
    }
    return true;
}

static inline bool needsRef(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_UnexposedExpr:
        return true;
    default:
        break;
    }
    return false;
}

CXChildVisitResult IndexerJob::indexVisitor(CXCursor cursor,
                                            CXCursor /*parent*/,
                                            CXClientData client_data)
{
    IndexerJob *job = static_cast<IndexerJob*>(client_data);
    if (job->isAborted())
        return CXChildVisit_Break;

    const CXCursorKind kind = clang_getCursorKind(cursor);

    const bool interesting = isInteresting(kind);
    if (testLog(VerboseDebug))
        verboseDebug() << "indexVisitor " << cursor << " " << clang_getCursorReferenced(cursor);
    if (kind >= CXCursor_FirstStmt && kind <= CXCursor_LastStmt)
        return CXChildVisit_Recurse;
    if (!interesting) {
        return CXChildVisit_Recurse;
    }

    CXCursor ref = clang_getCursorReferenced(cursor);
    const CXCursorKind refKind = clang_getCursorKind(ref);
    // the kind won't change even if the reference is looked up from elsewhere
    if (refKind == CXCursor_InvalidFile && needsRef(kind)) {
        return CXChildVisit_Recurse;
    }

    bool blocked = false;
    const Location loc = job->createLocation(cursor, &blocked);
    if (blocked) {
        switch (kind) {
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
        case CXCursor_Destructor:
        case CXCursor_Constructor:
            job->mHeaderMap[clang_getCursorUSR(cursor)] = cursor;
            break;
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
        case CXCursor_Namespace:
        case CXCursor_ClassTemplate:
        case CXCursor_UnexposedDecl:
            return CXChildVisit_Recurse;
        default:
            break;
        }
#if 0
        FILE *f = fopen("/tmp/smart", "r");
        if (f) {
            char line[1024];
            while ((RTags::readLine(f, line, sizeof(line))) != -1) {
                int id = atoi(line);
                if (id == kind) {
                    fclose(f);
                    return CXChildVisit_Recurse;
                }
            }
            fclose(f);
        }
#endif
        return CXChildVisit_Continue;
    } else if (loc.isNull()) {
        return CXChildVisit_Recurse;
    }

    /* CXCursor_CallExpr is the right thing to use for invocations of constructors */
    if (kind == CXCursor_CallExpr && (refKind == CXCursor_CXXMethod || refKind == CXCursor_FunctionDecl)) {
        return CXChildVisit_Recurse;
    }

    const Cursor c = { cursor, loc, kind };
    Location refLoc;
    if (!clang_equalCursors(cursor, ref)) {
        refLoc = job->createLocation(ref, 0);
    } else {
        if (!clang_isCursorDefinition(cursor)) {
            ref = clang_getCursorDefinition(cursor);
            if (!clang_equalCursors(nullCursor, ref)) {
                assert(!clang_equalCursors(cursor, ref));
                refLoc = job->createLocation(ref, 0);
                if (testLog(Debug)) {
                    debug() << "Looked up definition for ref " << ref << " " << cursor;
                }
            }
        }

        if (refLoc.isNull()) {
            const Cursor r = job->findByUSR(cursor, kind, loc);
            if (r.kind != CXCursor_FirstInvalid)
                return job->processCursor(c, r);
        }
    }
    const Cursor r = { ref, refLoc, refKind };
    return job->processCursor(c, r);
}

CXChildVisitResult IndexerJob::processCursor(const Cursor &cursor, const Cursor &ref)
{
    if (testLog(VerboseDebug))
        verboseDebug() << "processCursor " << cursor.cursor << " " << ref.cursor;

    if (cursor.kind == CXCursor_InclusionDirective) {
        CXFile includedFile = clang_getIncludedFile(cursor.cursor);
        if (includedFile) {
            const Location refLoc(includedFile, 0);
            if (!refLoc.isNull()) {
                {
                    ByteArray include = "#include ";
                    const Path path = refLoc.path();
                    mSymbolNames[(include + path)].insert(cursor.location);
                    mSymbolNames[(include + path.fileName())].insert(cursor.location);
                }
                CXSourceRange range = clang_getCursorExtent(cursor.cursor);
                unsigned int end;
                clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
                unsigned tokenCount = 0;
                CXToken *tokens = 0;
                clang_tokenize(mUnit, range, &tokens, &tokenCount);
                CursorInfo &info = mSymbols[cursor.location];
                info.target = refLoc;
                info.kind = cursor.kind;
                info.isDefinition = false;
                info.symbolLength = end - cursor.location.offset();
                assert(info.symbolLength > 0);
                for (unsigned i=0; i<tokenCount; ++i) {
                    if (clang_getTokenKind(tokens[i]) == CXToken_Literal) {
                        CXStringScope scope(clang_getTokenSpelling(mUnit, tokens[i]));
                        info.symbolName = "#include ";
                        info.symbolName += clang_getCString(scope.string);
                        mSymbolNames[info.symbolName].insert(cursor.location);
                        break;
                    }
                }
                if (tokens) {
                    clang_disposeTokens(mUnit, tokens, tokenCount);
                }
            }
        }
        return CXChildVisit_Recurse;
    }
    bool processRef = false;
    bool checkImplicit = false;
    switch (cursor.kind) {
    case CXCursor_MacroExpansion:
        processRef = (ref.kind == CXCursor_MacroDefinition);
        break;
    case CXCursor_TypeRef:
        switch (ref.kind) {
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
        case CXCursor_UnionDecl:
            if (clang_isCursorDefinition(ref.cursor))
                break;
            // fall through
        case CXCursor_TypedefDecl:
            processRef = true;
            break;
        default:
            break;
        }
    case CXCursor_DeclRefExpr:
    case CXCursor_UnexposedExpr:
        checkImplicit = (ref.kind == CXCursor_CXXMethod && RTags::eatString(clang_getCursorSpelling(ref.cursor)) == "operator=");
        break;
    case CXCursor_CallExpr:
        checkImplicit = (ref.kind == CXCursor_Constructor && RTags::eatString(clang_getCursorDisplayName(ref.cursor)).endsWith("()"));
        break;
    default:
        break;
    }
    if (processRef && !mSymbols.contains(ref.location)) {
        processCursor(ref, ref);
    }
    if (checkImplicit && clang_equalLocations(clang_getCursorLocation(ref.cursor),
                                              clang_getCursorLocation(clang_getCursorSemanticParent(ref.cursor)))) {
        debug() << "tossing reference to implicit cursor " << cursor.cursor << " " << ref.cursor;
        return CXChildVisit_Recurse;
    }
    const bool refOk = (!clang_isInvalid(ref.kind) && !ref.location.isNull() && ref.location != cursor.location);
    if (refOk && !isInteresting(ref.kind)) {
        debug() << "ref.kind is not interesting and cursor wants a ref " << cursor.cursor << " ref " << ref.cursor;
        return CXChildVisit_Recurse;
    }

    CursorInfo &info = mSymbols[cursor.location];
    if (!info.symbolLength) {
        if (mIsPch) {
            const ByteArray usr = RTags::eatString(clang_getCursorUSR(cursor.cursor));
            if (!usr.isEmpty()) {
                mPchUSRMap[usr] = cursor.location;
            }
        }
        info.isDefinition = clang_isCursorDefinition(cursor.cursor);
        info.kind = cursor.kind;
        const bool isReference = RTags::isReference(info.kind);

        CXStringScope name = clang_getCursorSpelling(cursor.cursor);
        const char *cstr = clang_getCString(name.string);
        info.symbolLength = cstr ? strlen(cstr) : 0;
        if (!info.symbolLength) {
            switch (info.kind) {
            case CXCursor_ClassDecl:
            case CXCursor_UnionDecl:
                info.symbolLength = 5;
                break;
            case CXCursor_StructDecl:
                info.symbolLength = 6;
                break;
            default:
                mSymbols.remove(cursor.location);
                return CXChildVisit_Recurse;
            }
        } else {
            info.symbolName = addNamePermutations(cursor.cursor, cursor.location, !isReference);
        }
        switch (info.kind) {
        case CXCursor_Constructor:
        case CXCursor_Destructor: {
            Location parentLocation = createLocation(clang_getCursorSemanticParent(cursor.cursor));
            // consider doing this for only declaration/inline definition since
            // declaration and definition should know of one another
            if (parentLocation.isValid()) {
                CursorInfo &parent = mSymbols[parentLocation];
                parent.additionalReferences.insert(cursor.location);
                info.additionalReferences.insert(parentLocation);
            }
            break; }

        default:
            break;
        }
    } else if (info.kind == CXCursor_Constructor && cursor.kind == CXCursor_TypeRef) {
        return CXChildVisit_Recurse;
    }

    if (refOk) {
        info.target = ref.location;
        RTags::ReferenceType referenceType = RTags::NormalReference;
        if (ref.kind == cursor.kind) {
            switch (ref.kind) {
            case CXCursor_Constructor:
            case CXCursor_Destructor:
            case CXCursor_CXXMethod:
                referenceType = RTags::MemberFunction;
                break;
            case CXCursor_FunctionDecl:
                referenceType = RTags::GlobalFunction;
                break;
            default:
                break;
            }
        }
        // ### For RTags we seem to get this count:
        // Duplicates: 18278 Non-duplicates: 69444 Overwrites: 2018
        // not sure if we should fix this.
        mReferences[cursor.location] = std::pair<Location, RTags::ReferenceType>(ref.location, referenceType);
    }
    return CXChildVisit_Recurse;
}

struct Scope {
    ~Scope()
    {
        cleanup();
    }
    void cleanup()
    {
        headerMap.clear();
        if (unit && !(flags & IndexerJob::PersistTranslationUnit)) {
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }
        if (index) {
            clang_disposeIndex(index);
            index = 0;
        }
    }

    Map<Str, CXCursor> &headerMap;
    CXTranslationUnit &unit;
    CXIndex &index;
    const unsigned flags;
};

void IndexerJob::run()
{
    execute();
    mFinished(this);
}

struct VerboseVisitorUserData
{
    int indent;
    ByteArray out;
    IndexerJob *job;
};
static inline CXChildVisitResult verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    CXCursor ref = clang_getCursorReferenced(cursor);

    VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
    u->out += ByteArray(u->indent, ' ');
    u->out += RTags::cursorToString(cursor);
    if (clang_equalCursors(ref, cursor)) {
        u->out += " refs self";
    } else if (!clang_equalCursors(ref, nullCursor)) {
        u->out += " refs " + RTags::cursorToString(ref);
    }

    Location loc = u->job->createLocation(cursor);
    if (loc.fileId() && u->job->mPaths.value(loc.fileId()) == IndexerJob::Index) {
        if (u->job->mReferences.contains(loc)) {
            u->out += " used as reference\n";
        } else if (u->job->mSymbols.contains(loc)) {
            u->out += " used as cursor\n";
        } else {
            u->out += " not used\n";
        }
    } else {
        u->out += " not indexed\n";
    }

    u->indent += 2;
    clang_visitChildren(cursor, verboseVisitor, userData);
    u->indent -= 2;
    return CXChildVisit_Continue;
}

void IndexerJob::execute()
{
    Timer timer;
    if (!mPchHeaders.isEmpty())
        mPchUSRMap = mIndexer->pchUSRMap(mPchHeaders.keys());

    List<const char*> clangArgs(mArgs.size(), 0);
    ByteArray clangLine = CLANG_BIN "clang ";
    bool nextIsX = false;
    ByteArray pchName;

    int idx = 0;
    const int count = mArgs.size();
    const Path projectRoot = mIndexer->projectRoot();
    for (int i=0; i<count; ++i) {
        ByteArray arg = mArgs.at(i);
        if (arg.isEmpty())
            continue;

        if (nextIsX) {
            nextIsX = false;
            mIsPch = (arg == "c++-header" || arg == "c-header");
        }
        if (arg == "-include-pch") {
            ++i;
            continue;
        }
        clangArgs[idx++] = arg.constData();
        arg.replace("\"", "\\\"");
        clangLine += arg;
        clangLine += " ";
        if (arg == "-x") {
            nextIsX = true;
        }
    }
    for (Map<Path, Path>::const_iterator it = mPchHeaders.begin(); it != mPchHeaders.end(); ++it) {
        clangArgs[idx++] = "-include-pch";
        clangArgs[idx++] = it->second.constData();
    }

    if (mIsPch) {
        pchName = pchFileName(projectRoot, mIn);
    }
    clangLine += mIn;

    if (isAborted()) {
        return;
    }
    CXIndex index = clang_createIndex(1, 0);
    mUnit = clang_parseTranslationUnit(index, mIn.constData(),
                                       clangArgs.data(), idx, 0, 0,
                                       CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);
    Scope scope = { mHeaderMap, mUnit, index, mFlags };
    const time_t timeStamp = time(0);
    // fprintf(stdout, "%s => %d\n", clangLine.nullTerminated(), (mUnit != 0));

    warning() << "loading unit " << clangLine << " " << (mUnit != 0);
    if (isAborted()) {
        return;
    }

    mDependencies[mFileId].insert(mFileId);
    const Path srcRoot = mIndexer->srcRoot();
    // error() << "writing file information " << mFileId << " " << mIn;
    writeFileInformation(mFileId, mArgs, timeStamp,
                         Server::instance()->db(Server::FileInformation, ReadWriteLock::Write, srcRoot));
    bool compileError = false;
    if (!mUnit) {
        compileError = true;
        error() << "got 0 unit for " << clangLine;
        mIndexer->addDependencies(mDependencies);
    } else {
        Map<Location, std::pair<int, ByteArray> > fixIts;
        Map<uint32_t, List<ByteArray> > visited;
        const unsigned diagnosticCount = clang_getNumDiagnostics(mUnit);
        bool hasCompilationErrors = false;
        for (unsigned i=0; i<diagnosticCount; ++i) {
            CXDiagnostic diagnostic = clang_getDiagnostic(mUnit, i);
            int logLevel = INT_MAX;
            const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);
            switch (severity) {
            case CXDiagnostic_Fatal:
            case CXDiagnostic_Error:
                logLevel = Error;
                hasCompilationErrors = true;
                break;
            case CXDiagnostic_Warning:
                logLevel = Warning;
                hasCompilationErrors = true;
                break;
            case CXDiagnostic_Note:
                logLevel = Debug;
                break;
            case CXDiagnostic_Ignored:
                break;
            }

            if (testLog(logLevel) || (logLevel >= Warning && testLog(CompilationError))) {
                CXSourceLocation loc = clang_getDiagnosticLocation(diagnostic);
                const ByteArray string = RTags::eatString(clang_formatDiagnostic(diagnostic,
                                                                                 CXDiagnostic_DisplaySourceLocation|
                                                                                 CXDiagnostic_DisplayColumn|
                                                                                 CXDiagnostic_DisplaySourceRanges|
                                                                                 CXDiagnostic_DisplayOption|
                                                                                 CXDiagnostic_DisplayCategoryId|
                                                                                 CXDiagnostic_DisplayCategoryName));
                CXFile file;
                clang_getSpellingLocation(loc, &file, 0, 0, 0);
                if (file)
                    visited[Location(file, 0).fileId()].append(string);

                log(logLevel, "%s: %s => %s", mIn.constData(), string.constData(), clangLine.constData());
                log(CompilationError, "%s", string.constData());
            }

            const unsigned fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
            for (unsigned f=0; f<fixItCount; ++f) {
                CXSourceRange range;
                CXString string = clang_getDiagnosticFixIt(diagnostic, f, &range);
                const Location start(clang_getRangeStart(range));
                unsigned endOffset = 0;
                clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &endOffset);

                error("Fixit (%d/%d) for %s: [%s] %s-%d", f + 1, fixItCount, mIn.constData(),
                      clang_getCString(string), start.key().constData(), endOffset);
                // ### can there be more than one fixit starting at the same location? Probably not.
                fixIts[start] = std::pair<int, ByteArray>(endOffset - start.offset(), RTags::eatString(string));
            }

            clang_disposeDiagnostic(diagnostic);
        }
        if (!hasCompilationErrors) {
            log(CompilationError, "%s parsed", mIn.constData());
        }

        clang_getInclusions(mUnit, inclusionVisitor, this);

        clang_visitChildren(clang_getTranslationUnitCursor(mUnit), indexVisitor, this);
        if (testLog(VerboseDebug)) {
            VerboseVisitorUserData u = { 0, "<VerboseVisitor " + clangLine + ">", this };
            clang_visitChildren(clang_getTranslationUnitCursor(mUnit), verboseVisitor, &u);
            u.out += "</VerboseVisitor " + clangLine + ">";
            logDirect(VerboseDebug, u.out);
        }

        if (mIsPch) {
            assert(!pchName.isEmpty());
            if (clang_saveTranslationUnit(mUnit, pchName.constData(), clang_defaultSaveOptions(mUnit)) != CXSaveError_None) {
                error() << "Couldn't save pch file" << mIn << pchName;
            } else {
                mIndexer->setPchUSRMap(mIn, mPchUSRMap);
            }
        }
        for (Map<Path, Path>::const_iterator it = mPchHeaders.begin(); it != mPchHeaders.end(); ++it) {
            const Path &pchHeader = it->first;
            const Set<uint32_t> pchDeps = mIndexer->pchDependencies(pchHeader);
            for (Set<uint32_t>::const_iterator it = pchDeps.begin(); it != pchDeps.end(); ++it) {
                mDependencies[*it].insert(mFileId);
            }
        }
        scope.cleanup();

        if (!isAborted()) {
            Set<uint32_t> indexed;
            Set<uint32_t> referenced;
            for (Map<uint32_t, PathState>::const_iterator it = mPaths.begin(); it != mPaths.end(); ++it) {
                if (it->second == Index) {
                    visited[it->first] = List<ByteArray>();
                    if (mFlags & (DirtyPch|Dirty)) {
                        indexed.insert(it->first);
                    }
                } else if (mFlags & (DirtyPch|Dirty) && it->second == Reference) {
                    referenced.insert(it->first);
                }
            }
            mIndexer->addDependencies(mDependencies);
            assert(mDependencies[mFileId].contains(mFileId));

            mIndexer->setDiagnostics(visited, fixIts);
            writeSymbols(mSymbols, mReferences, Server::instance()->db(Server::Symbol, ReadWriteLock::Write, srcRoot));

            writeSymbolNames(mSymbolNames, Server::instance()->db(Server::SymbolName, ReadWriteLock::Write, srcRoot));
            if (mIsPch)
                mIndexer->setPchDependencies(mIn, mPchDependencies);
        }
    }

    char buf[1024];
    const char *strings[] = { "", " (pch)", " (dirty)", " (pch, dirty)" };
    enum {
        IdxNone = 0x0,
        IdxPch = 0x1,
        IdxDirty = 0x2
    };
    const int w = snprintf(buf, sizeof(buf), "Visited %s (%s) in %sms. (%d syms, %d refs, %d deps, %d symNames)%s",
                           mIn.constData(), compileError ? "error" : "success", ByteArray::number(timer.elapsed()).constData(),
                           mSymbols.size(), mReferences.size(), mDependencies.size(), mSymbolNames.size(),
                           strings[(mPchHeaders.isEmpty() ? IdxNone : IdxPch) | (mFlags & (DirtyPch|Dirty) ? IdxDirty : IdxNone)]);
    mMessage = ByteArray(buf, w);
    if (testLog(Warning)) {
        warning() << "We're using " << double(MemoryMonitor::usage()) / double(1024 * 1024) << " MB of memory " << timer.elapsed() << "ms";
    }
}

CXChildVisitResult isInlineVisitor(CXCursor, CXCursor, CXClientData u)
{
    *reinterpret_cast<bool*>(u) = true;
    return CXChildVisit_Break;
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

IndexerJob::Cursor IndexerJob::findByUSR(const CXCursor &cursor, CXCursorKind kind, const Location &loc)
{
    bool ok = false;
    switch (kind) {
    case CXCursor_FunctionDecl:
        ok = (clang_isCursorDefinition(cursor) && loc.fileId() == mFileId);
        break;
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor:
        ok = (clang_isCursorDefinition(cursor) && loc.fileId() == mFileId && !isInline(cursor));
        break;
    default:
        break;
    }
    if (!ok) {
        const Cursor ret = { nullCursor, Location(), CXCursor_FirstInvalid };
        return ret;
    }

    const Str usr(clang_getCursorUSR(cursor));
    if (!usr.length()) {
        const Cursor ret = { nullCursor, Location(), CXCursor_FirstInvalid };
        return ret;
    }

    const ByteArray key(usr.data(), usr.length());
    Location refLoc = mPchUSRMap.value(key);
    if (!refLoc.isNull()) {
        const Cursor ret = { cursor, refLoc, clang_getCursorKind(cursor) };
        // ### even if this isn't the right CXCursor it's good enough for our needs
        return ret;
    }

    Map<Str, CXCursor>::const_iterator it = mHeaderMap.find(usr);
    if (it != mHeaderMap.end()) {
        const CXCursor ref = it->second;
        const Cursor ret = { ref, createLocation(ref, 0), clang_getCursorKind(ref) };
        assert(!clang_equalCursors(ref, cursor)); // ### why is this happening?
        return ret;
    }
    const Cursor ret = { nullCursor, Location(), CXCursor_FirstInvalid };
    return ret;
}
