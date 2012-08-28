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
            const Map<Location, RTags::ReferenceType> refs = it->second;
            for (Map<Location, RTags::ReferenceType>::const_iterator rit = refs.begin(); rit != refs.end(); ++rit) {
                CursorInfo &ci = symbols[rit->first];
                if (rit->second != RTags::NormalReference) {
                    CursorInfo &other = symbols[it->first];
                    // error() << "trying to join" << it->first << "and" << it->second.front();
                    if (other.target.isNull())
                        other.target = rit->first;
                    if (ci.target.isNull())
                        ci.target = it->first;
                } else {
                    ci.references.insert(it->first);
                }
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

IndexerJob::IndexerJob(Indexer *indexer, unsigned flags, const Path &input, const List<ByteArray> &arguments)
    : mFlags(flags), mTimeStamp(time(0)), mIsPch(false), mIn(input),
      mFileId(Location::insertFile(input)), mArgs(arguments), mIndexer(indexer),
      mPchHeaders(extractPchFiles(mIndexer->projectPath(), arguments)), mUnit(0)
{
    setAutoDelete(true);
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
        bool hasTemplates = false;
        switch (kind) {
        case CXCursor_ClassTemplate:
        case CXCursor_Constructor:
        case CXCursor_Destructor:
            hasTemplates = qnoparam.contains('<');
            break;
        default:
            break;
        }

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
                    state = mIndexer->visitFile(fileId, mIn) ? Index : DontIndex;
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

CXChildVisitResult IndexerJob::indexVisitor(CXCursor cursor,
                                            CXCursor /*parent*/,
                                            CXClientData client_data)
{
    IndexerJob *job = static_cast<IndexerJob*>(client_data);
    if (job->isAborted())
        return CXChildVisit_Break;

    const CXCursorKind kind = clang_getCursorKind(cursor);
    if (clang_isStatement(kind))
        return CXChildVisit_Recurse;

    enum Type {
        Include,
        Cursor,
        Reference,
        Other
    } type = Other;
    if (RTags::isCursor(kind)) {
        type = Cursor;
    } else if (RTags::isReference(kind)) {
        type = Reference;
    } else if (kind == CXCursor_InclusionDirective) {
        type = Include;
    } else {
        return CXChildVisit_Recurse; // ### continue
    }


    bool blocked = false;
    const Location loc = job->createLocation(cursor, &blocked);
    if (blocked) {
        switch (kind) {
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
        case CXCursor_Destructor:
        case CXCursor_Constructor:
            job->mHeaderMap[clang_getCursorUSR(cursor)] = job->createLocation(cursor, 0);
            return CXChildVisit_Continue;
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
        case CXCursor_Namespace:
        case CXCursor_ClassTemplate:
        case CXCursor_UnexposedDecl:
            return CXChildVisit_Recurse;
        default:
            return CXChildVisit_Continue;
        }
    } else if (loc.isNull()) {
        return CXChildVisit_Continue;
    } else if (job->mSymbols.value(loc).symbolLength) {
        return CXChildVisit_Recurse;
    }

    // if (loc == "/usr/include/getopt.h,3843" || loc == "/home/abakken/dev/rtags/src/RTags.h,5430" || loc == "/home/abakken/dev/rtags/src/rdm.cpp,3970") {
    //     error() << "got some stuff here" << cursor << type << job->mIn
    //             << clang_getCursorReferenced(cursor);
    // }

    switch (type) {
    case Cursor:
        job->handleCursor(cursor, kind, loc);
        break;
    case Include:
        job->handleInclude(cursor, kind, loc);
        break;
    case Reference:
        job->handleReference(cursor, kind, loc);
        break;
    case Other:
        assert(0);
        break;
    }
    return CXChildVisit_Recurse; // ### recurse?
}

// Could set a flag when we get to template functions and continue but visit with a different visitor
// #warning templatized functions only get indexed if called seemingly, maybe even the same with inline ones. If so, the file that wins it could end up not using it
static inline bool inTemplateFunction(CXCursor cursor)
{
    while (true) {
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
            break;
        case CXCursor_TranslationUnit:
            return false;
        default:
            cursor = clang_getCursorSemanticParent(cursor); // lexical?
            break;
        }
    }
    return true;
}

void IndexerJob::handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &loc)
{
    CXCursor ref = clang_getCursorReferenced(cursor);
    const CXCursorKind refKind = clang_getCursorKind(ref);
    if (clang_isInvalid(refKind))
        return;

    bool checkImplicit = false;
    bool processRef = false;

    switch (kind) {
    case CXCursor_CallExpr:
        /* CXCursor_CallExpr is the right thing to use for invocations of
         * constructors but for CXXMethod it causes problems. Specifically because
         * the ref is position on the member and is visited before the DeclRefExpr
         * which we want.
         */
        switch (refKind) {
        case CXCursor_CXXMethod:
        case CXCursor_FunctionDecl:
            return;
        case CXCursor_Constructor: {
            CXStringScope scope = clang_getCursorDisplayName(ref);
            const char *data = scope.data();
            const int len = data ? strlen(data) : 0;
            if (len > 2 && !strncmp(data + len - 2, "()", 2)) {
                checkImplicit = true;
            }
            break; }
        default:
            break;
        }
    case CXCursor_DeclRefExpr:
    case CXCursor_UnexposedExpr:
        if (refKind == CXCursor_CXXMethod) {
            CXStringScope scope = clang_getCursorDisplayName(ref);
            const char *data = scope.data();
            if (data && !strncmp(data, "operator", 8))
                checkImplicit = true;
        }
        break;
    case CXCursor_TypeRef:
        switch (refKind) {
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
        case CXCursor_UnionDecl:
        case CXCursor_TemplateTypeParameter:
        case CXCursor_TypedefDecl:
            processRef = true;
            break;
        default:
            break;
        }
        break;
    case CXCursor_MacroExpansion:
        processRef = true;
        break;
    default:
        break;
    }

    const Location refLoc = createLocation(ref, 0);
    if (!refLoc.isValid())
        return;

    if (checkImplicit && clang_equalLocations(clang_getCursorLocation(ref),
                                              clang_getCursorLocation(clang_getCursorSemanticParent(ref)))) {
        debug() << "tossing reference to implicit cursor " << cursor << " " << ref;
        return;
    }

    if (processRef) {
        if (!mSymbols.value(refLoc).symbolLength)
            handleCursor(ref, refKind, refLoc);
    }

    handleCursor(cursor, kind, loc, &refLoc);
}

void IndexerJob::addOverriddenCursors(const CXCursor& cursor, const Location& location, List<CursorInfo*>& infos)
{
    CXCursor *overridden;
    unsigned count;
    clang_getOverriddenCursors(cursor, &overridden, &count);
    if (!overridden)
        return;
    for (unsigned i=0; i<count; ++i) {
        Location loc = createLocation(overridden[i], 0);
        CursorInfo &o = mSymbols[loc];

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

void IndexerJob::handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location)
{
    assert(kind == CXCursor_InclusionDirective);
    (void)kind;
    CXFile includedFile = clang_getIncludedFile(cursor);
    if (includedFile) {
        const Location refLoc(includedFile, 0);
        if (!refLoc.isNull()) {
            {
                ByteArray include = "#include ";
                const Path path = refLoc.path();
                mSymbolNames[(include + path)].insert(location);
                mSymbolNames[(include + path.fileName())].insert(location);
            }
            CXSourceRange range = clang_getCursorExtent(cursor);
            unsigned int end;
            clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
            unsigned tokenCount = 0;
            CXToken *tokens = 0;
            clang_tokenize(mUnit, range, &tokens, &tokenCount);
            CursorInfo &info = mSymbols[location];
            info.target = refLoc;
            info.kind = cursor.kind;
            info.isDefinition = false;
            info.symbolLength = end - location.offset();
            assert(info.symbolLength > 0);
            for (unsigned i=0; i<tokenCount; ++i) {
                if (clang_getTokenKind(tokens[i]) == CXToken_Literal) {
                    CXStringScope scope(clang_getTokenSpelling(mUnit, tokens[i]));
                    info.symbolName = "#include ";
                    info.symbolName += clang_getCString(scope.string);
                    mSymbolNames[info.symbolName].insert(location);
                    break;
                }
            }
            if (tokens) {
                clang_disposeTokens(mUnit, tokens, tokenCount);
            }
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

void IndexerJob::handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location, const Location *ref)
{
    // if (location == "/home/abakken/dev/rtags/src/rc.cpp,8221")
    //     error() << cursor << "refs" << (ref ? *ref : clang_getNullCursor()) << mIn;

    CursorInfo &info = mSymbols[location];
    if (info.symbolLength) {
        // error() << "current" << info << "\nnew" << cursor << (ref ? *ref : clang_getNullCursor());
        return;
    }
    if (!info.symbolLength) {
        info.isDefinition = clang_isCursorDefinition(cursor);
        info.kind = kind;
        const bool isReference = RTags::isReference(kind);

        CXStringScope name = clang_getCursorSpelling(cursor);
        const char *cstr = clang_getCString(name.string);
        info.symbolLength = cstr ? strlen(cstr) : 0;
        if (!info.symbolLength) {
            switch (kind) {
            case CXCursor_ClassDecl:
            case CXCursor_UnionDecl:
                info.symbolLength = 5;
                break;
            case CXCursor_StructDecl:
                info.symbolLength = 6;
                break;
            default:
                mSymbols.remove(location);
                return;
            }
        } else {
            info.symbolName = addNamePermutations(cursor, location, !isReference);
        }
    }
    RTags::ReferenceType referenceType = RTags::NormalReference;
    switch (info.kind) {
    case CXCursor_Constructor:
    case CXCursor_Destructor: {
        referenceType = RTags::MemberFunction;
        Location parentLocation = createLocation(clang_getCursorSemanticParent(cursor));
        // consider doing this for only declaration/inline definition since
        // declaration and definition should know of one another
        if (parentLocation.isValid()) {
            CursorInfo &parent = mSymbols[parentLocation];
            parent.references.insert(location);
            info.references.insert(parentLocation);
        }
        break; }
    case CXCursor_CXXMethod: {
        referenceType = RTags::MemberFunction;
        List<CursorInfo*> infos;
        infos.append(&info);
        addOverriddenCursors(cursor, location, infos);
        break; }
    case CXCursor_FunctionDecl:
        referenceType = RTags::GlobalFunction;
        break;
    default:
        break;
    }
    Location refLoc;
    if (ref) {
        refLoc = *ref;
        assert(refLoc.isValid());
    } else if (referenceType != RTags::NormalReference) {
        if (info.isDefinition) {
            bool ok = false;
            switch (kind) {
            case CXCursor_FunctionDecl:
                ok = (location.fileId() == mFileId);
                break;
            case CXCursor_CXXMethod:
            case CXCursor_Destructor:
            case CXCursor_Constructor:
                ok = (location.fileId() == mFileId && !isInline(cursor));
                break;
            default:
                assert(0);
                break;
            }
            if (ok) {
                const Str usr(clang_getCursorUSR(cursor));
                if (usr.length()) {
                    refLoc = mHeaderMap.value(usr);
                }
            }
        } else {
            CXCursor other = clang_getCursorDefinition(cursor);
            if (!clang_equalCursors(nullCursor, other)) {
                refLoc = createLocation(other, 0);
                assert(!clang_equalCursors(cursor, other));
            }
        }
    }

    if (refLoc.isValid()) {
        Map<Location, RTags::ReferenceType> &val = mReferences[location];
        val[refLoc] = referenceType;
        info.target = refLoc;
    }
}

struct Scope {
    ~Scope()
    {
        cleanup();
    }
    void cleanup()
    {
        headerMap.clear();
        if (unit) {
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }
        if (index) {
            clang_disposeIndex(index);
            index = 0;
        }
    }

    Map<Str, Location> &headerMap;
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
    VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
    Location loc = u->job->createLocation(cursor);
    if (loc.fileId()) {
        CXCursor ref = clang_getCursorReferenced(cursor);

        VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
        if (u->indent >= 0)
            u->out += ByteArray(u->indent, ' ');
        u->out += RTags::cursorToString(cursor);
        if (clang_equalCursors(ref, cursor)) {
            u->out += " refs self";
        } else if (!clang_equalCursors(ref, nullCursor)) {
            u->out += " refs " + RTags::cursorToString(ref);
        }

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
    }
    if (u->indent >= 0) {
        u->indent += 2;
        clang_visitChildren(cursor, verboseVisitor, userData);
        u->indent -= 2;
        return CXChildVisit_Continue;
    } else {
        return CXChildVisit_Recurse;
    }
}

void IndexerJob::execute()
{
    shared_ptr<Project> project = mIndexer->project();
    assert(project);
    Timer timer;

    List<const char*> clangArgs(mArgs.size(), 0);
    ByteArray clangLine = Server::instance()->clangPath();
    bool nextIsX = false;
    ByteArray pchName;

    int idx = 0;
    const int count = mArgs.size();
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
        pchName = pchFileName(mIndexer->projectPath(), mIn);
    }
    clangLine += mIn;

    if (isAborted()) {
        return;
    }

    CXIndex index = clang_createIndex(0, 1);
    mUnit = clang_parseTranslationUnit(index, mIn.constData(),
                                       clangArgs.data(), idx, 0, 0,
                                       CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);
    Scope scope = { mHeaderMap, mUnit, index, mFlags };

    warning() << "loading unit " << clangLine << " " << (mUnit != 0);
    if (isAborted()) {
        return;
    }

    mDependencies[mFileId].insert(mFileId);
    const Path srcRoot = mIndexer->srcRoot();
    writeFileInformation(mFileId, mArgs, mTimeStamp,
                         mIndexer->project()->db(Project::FileInformation, ReadWriteLock::Write));

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

        if (!mIsPch) {
            clang_visitChildren(clang_getTranslationUnitCursor(mUnit), indexVisitor, this);
            if (testLog(VerboseDebug)) {
                {
                    VerboseVisitorUserData u = { 0, "<VerboseVisitor " + clangLine + ">", this };
                    clang_visitChildren(clang_getTranslationUnitCursor(mUnit), verboseVisitor, &u);
                    u.out += "</VerboseVisitor " + clangLine + ">";
                    char buf[1024];
                    snprintf(buf, sizeof(buf), "/tmp/%s.log", mIn.fileName());
                    FILE *f = fopen(buf, "w");
                    assert(f);
                    fwrite(u.out.constData(), 1, u.out.size(), f);
                    fclose(f);
                    // logDirect(VerboseDebug, u.out);
                }
                // {
                //     VerboseVisitorUserData u = { -1, "<VerboseVisitor2 " + clangLine + ">", this };
                //     clang_visitChildren(clang_getTranslationUnitCursor(mUnit), verboseVisitor, &u);
                //     u.out += "</VerboseVisitor2 " + clangLine + ">";
                //     logDirect(VerboseDebug, u.out);
                // }
            }
        }
        if (mIsPch) {
            assert(!pchName.isEmpty());
            if (clang_saveTranslationUnit(mUnit, pchName.constData(), clang_defaultSaveOptions(mUnit)) != CXSaveError_None) {
                error() << "Couldn't save pch file" << mIn << pchName;
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
            writeSymbols(mSymbols, mReferences, project->db(Project::Symbol, ReadWriteLock::Write));

            writeSymbolNames(mSymbolNames, project->db(Project::SymbolName, ReadWriteLock::Write));
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

