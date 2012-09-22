#include "IndexerJob.h"
#include "Timer.h"
#include "MemoryMonitor.h"
#include "Server.h"
#include "EventLoop.h"

struct DumpUserData {
    int indent;
    IndexerJob *job;
};

struct FindImplicitEqualsConstructorUserData {
    CXCursor &ref;
    bool &success;
};

struct VerboseVisitorUserData {
    int indent;
    ByteArray out;
    IndexerJob *job;
};

IndexerJob::IndexerJob(const shared_ptr<Indexer> &indexer, unsigned flags, const Path &p, const List<ByteArray> &arguments)
    : Job(0, indexer->project()),
      mFlags(flags), mTimeStamp(0), mPath(p), mFileId(Location::insertFile(p)),
      mArgs(arguments), mIndexer(indexer), mUnit(0), mIndex(0),
      mIgnoreConstructorRefs(false)
{
}

IndexerJob::IndexerJob(const QueryMessage &msg, const shared_ptr<Project> &project,
                       const Path &input, const List<ByteArray> &arguments)
    : Job(msg, WriteUnfiltered|WriteBuffered, project), mFlags(0), mTimeStamp(0), mPath(input), mFileId(Location::insertFile(input)),
      mArgs(arguments), mUnit(0), mIndex(0), mIgnoreConstructorRefs(false)
{
}

void IndexerJob::inclusionVisitor(CXFile includedFile,
                                  CXSourceLocation *includeStack,
                                  unsigned includeLen,
                                  CXClientData userData)
{
    IndexerJob *job = static_cast<IndexerJob*>(userData);
    const Location l(includedFile, 0);

    const Path path = l.path();
    job->mData->symbolNames[path].insert(l);
    const char *fn = path.fileName();
    job->mData->symbolNames[ByteArray(fn, strlen(fn))].insert(l);

    const uint32_t fileId = l.fileId();
    if (!includeLen) {
        job->mData->dependencies[fileId].insert(fileId);
    } else {
        for (unsigned i=0; i<includeLen; ++i) {
            CXFile originatingFile;
            clang_getSpellingLocation(includeStack[i], &originatingFile, 0, 0, 0);
            Location loc(originatingFile, 0);
            const uint32_t f = loc.fileId();
            if (f)
                job->mData->dependencies[fileId].insert(f);
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

        addToSymbolNames(qparam, hasTemplates, location, mData->symbolNames);
        if (!qnoparam.isEmpty()) {
            assert(!qnoparam.isEmpty());
            addToSymbolNames(qnoparam, hasTemplates, location, mData->symbolNames);
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
    if (blocked)
        *blocked = false;
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
                    shared_ptr<Indexer> indexer = mIndexer.lock();
                    state = indexer && indexer->visitFile(fileId, this) ? Index : DontIndex;
                }
                if (state != Index) {
                    *blocked = true;
                    return Location();
                }
            }
        }
    }
    return ret;
}

static CXChildVisitResult findImplicitEqualsConstructor(CXCursor cursor, CXCursor, CXClientData data)
{
    if (clang_getCursorKind(cursor) == CXCursor_UnexposedExpr) {
        const CXCursor ref = clang_getCursorReferenced(cursor);
        const CXCursorKind kind = clang_getCursorKind(ref);
        if (kind == CXCursor_Constructor) {
            FindImplicitEqualsConstructorUserData *u = reinterpret_cast<FindImplicitEqualsConstructorUserData*>(data);
            u->success = true;
            u->ref = ref;
        } else if (clang_isInvalid(clang_getCursorKind(ref))) {
            return CXChildVisit_Recurse;
        }
    }

    return CXChildVisit_Break;
}

CXChildVisitResult IndexerJob::indexVisitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
    IndexerJob *job = static_cast<IndexerJob*>(data);
    const CXCursorKind kind = clang_getCursorKind(cursor);
    const RTags::CursorType type = RTags::cursorType(kind);
    if (type == RTags::Other)
        return CXChildVisit_Recurse;

    bool blocked = false;
    Location loc = job->createLocation(cursor, &blocked);
    if (blocked) {
        switch (kind) {
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
        case CXCursor_Destructor:
        case CXCursor_Constructor:
        case CXCursor_VarDecl:
        case CXCursor_FunctionTemplate:
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
        job->handleCursor(cursor, kind, loc);
        break;
    case RTags::Include:
        job->handleInclude(cursor, kind, loc);
        break;
    case RTags::Reference: {
        CXCursor ref = clang_getCursorReferenced(cursor);
        CXCursorKind refKind = clang_getCursorKind(ref);
        if (clang_isInvalid(refKind)) {
            bool implicitConstructorSpecialCase = false;
            // terrible hack for these kinds of constructors:
            // struct Foo { Foo(int); };
            // Foo f = 12;
            // The cursors just mess us up, the first CallExpr has no referenced
            // cursor. The later cursors (callexpr and unexposed expr refer to
            // the actual constructor but are positioned at the 12 instead of at
            // the f. This is just completely fucked but I can't think of any
            // other way to do this.
            if (kind == CXCursor_CallExpr && clang_getCursorKind(parent) == CXCursor_VarDecl) {
                FindImplicitEqualsConstructorUserData userData = { ref, implicitConstructorSpecialCase };
                clang_visitChildren(cursor, findImplicitEqualsConstructor, &userData);
            }
            if (!implicitConstructorSpecialCase) {
                return CXChildVisit_Recurse;
            } else {
                refKind = CXCursor_Constructor;
                job->handleReference(cursor, kind, loc, ref, refKind, parent);
                job->mIgnoreConstructorRefs = true;
                clang_visitChildren(cursor, indexVisitor, job);
                job->mIgnoreConstructorRefs = false;
                return CXChildVisit_Continue;
            }
        } else if (kind == CXCursor_CallExpr && refKind == CXCursor_Constructor
                   && clang_getCursorKind(parent) == CXCursor_VarDecl) {
            // Terrible hack for these kinds of copy constructors:
            // struct Foo { Foo(const Foo &f); };
            // Foo f;
            // Foo f2 = f;

            // The call expr has the right reference but its location is on the
            // f rather than on f2. We can fix this by taking the location from
            // our parent.
            loc = job->createLocation(parent, 0);
        }
        job->handleReference(cursor, kind, loc, ref, refKind, parent);
        break; }
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

void IndexerJob::handleReference(const CXCursor &cursor, CXCursorKind kind,
                                 const Location &loc, const CXCursor &ref, CXCursorKind refKind, const CXCursor &parent)
{
    assert(!clang_isInvalid(refKind));
    if (mIgnoreConstructorRefs && refKind == CXCursor_Constructor)
        return;

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
            if (len > 2 && !strncmp(data + len - 2, "()", 2) && isImplicit(ref))
                return;

            if (clang_getCursorKind(parent) == CXCursor_CXXFunctionalCastExpr) {
                SymbolMap::iterator it = mData->symbols.find(loc);
                if (it != mData->symbols.end() && it->second.kind == CXCursor_TypeRef) {
                    const CXCursorKind existingRefKind = mData->symbols.value(it->second.target).kind;
                    if (existingRefKind == CXCursor_FirstInvalid || existingRefKind == CXCursor_Constructor) {
                        it->second.symbolLength = 0;
                        it->second.target.clear();
                    }
                    // The typeRef comes first but we want the callexpr to be the cursor
                    // e.g.
                    // struct Foo { Foo(int) };
                    // Foo(12); => refers to struct Foo instead of Foo(int)
                    // we do want the reference on struct Foo for renaming reasons
                }
            }
            break; }
        default:
            break;
        }
    case CXCursor_TemplateRef:
        processRef = (refKind == CXCursor_ClassTemplate);
        break;
    case CXCursor_DeclRefExpr:
        switch (refKind) {
        case CXCursor_FunctionDecl:
        case CXCursor_NonTypeTemplateParameter:
            processRef = true;
            break;
        case CXCursor_VarDecl: {
            SymbolMap::iterator it = mData->symbols.find(loc);
            if (it != mData->symbols.end() && it->second.kind == CXCursor_CallExpr) {
                const CXCursorKind existingRefKind = mData->symbols.value(it->second.target).kind;
                if (existingRefKind == CXCursor_FirstInvalid || existingRefKind == CXCursor_Constructor) {
                    it->second.symbolLength = 0;
                    it->second.target.clear();
                    // terrible hack for things like this:
                    // struct Foo { Foo(int) {} Foo(const Foo &) {} };
                    // void foo(Foo foo); // note the missing &
                    // Foo f;
                    // foo(f);
                    //
                    // without this hack the argument in foo(f) would refer to Foo(int) rather than the variable f;
                    // note that we do want the reference to Foo(const Foo &)
                }
            }
            break; }
        default:
            break;
        }
        // fall through
    case CXCursor_UnexposedExpr:
        if (refKind == CXCursor_CXXMethod) {
            CXStringScope scope = clang_getCursorDisplayName(ref);
            const char *data = scope.data();
            if (data && !strncmp(data, "operator", 8) && isImplicit(ref))
                return;
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
    case CXCursor_MemberRefExpr:
        processRef = (refKind == CXCursor_FieldDecl);
        break;
    case CXCursor_MacroExpansion:
    case CXCursor_LabelRef:
        processRef = true;
        break;
    default:
        break;
    }

    const Location refLoc = createLocation(ref, 0);
    if (!refLoc.isValid())
        return;

    if (processRef && !mData->symbols.value(refLoc).symbolLength)
        handleCursor(ref, refKind, refLoc);

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
                mData->symbolNames[(include + path)].insert(location);
                mData->symbolNames[(include + path.fileName())].insert(location);
            }
            CursorInfo &info = mData->symbols[location];
            info.target = refLoc;
            info.kind = cursor.kind;
            info.isDefinition = false;
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

void IndexerJob::handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location, const Location *ref)
{
    CursorInfo &info = mData->symbols[location];
    RTags::ReferenceType referenceType = ref ? RTags::NormalReference : RTags::NoReference;
    Location refLoc;
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
                mData->symbols.remove(location);
                return;
            }
        } else {
            info.symbolName = addNamePermutations(cursor, location, !isReference);
        }
        switch (info.kind) {
        case CXCursor_Constructor:
        case CXCursor_Destructor: {
            referenceType = RTags::LinkedReference;
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
            referenceType = RTags::LinkedReference;
            List<CursorInfo*> infos;
            infos.append(&info);
            addOverriddenCursors(cursor, location, infos);
            break; }
        case CXCursor_FunctionTemplate:
        case CXCursor_FunctionDecl:
        case CXCursor_VarDecl:
            referenceType = RTags::LinkedReference;
            break;
        default:
            break;
        }
        if (referenceType != RTags::NoReference) {
            if (info.isDefinition) {
                switch (kind) {
                case CXCursor_CXXMethod:
                case CXCursor_Destructor:
                case CXCursor_Constructor:
                case CXCursor_FunctionTemplate:
                    if (isInline(cursor))
                        break;
                    // fall throughg
                case CXCursor_FunctionDecl:
                case CXCursor_VarDecl: {
                    const Str usr(clang_getCursorUSR(cursor));
                    if (usr.length()) {
                        refLoc = mHeaderMap.value(usr);
                    }
                    break; }
                default:
                    assert(0);
                    break;
                }
            } else {
                CXCursor other = clang_getCursorDefinition(cursor);
                if (!clang_equalCursors(nullCursor, other)) {
                    refLoc = createLocation(other, 0);
                    assert(!clang_equalCursors(cursor, other));
                }
            }
        }
    }
    if (ref) {
        refLoc = *ref;
        assert(refLoc.isValid());
    }

    if (refLoc.isValid()) {
        Map<Location, RTags::ReferenceType> &val = mData->references[location];
        val[refLoc] = referenceType;
        if (info.target.isNull())
            info.target = refLoc;
    }
}

void IndexerJob::parse()
{
    mHeaderMap.clear();
    if (!mIndex) {
        mIndex = clang_createIndex(0, 1);
        if (!mIndex) {
            abort();
            return;
        }
    }

    mTimeStamp = time(0);
    List<const char*> clangArgs(mArgs.size(), 0);
    mClangLine = Server::instance()->clangPath();
    mClangLine += ' ';

    int idx = 0;
    const int count = mArgs.size();
    for (int i=0; i<count; ++i) {
        ByteArray arg = mArgs.at(i);
        if (arg.isEmpty())
            continue;

        clangArgs[idx++] = arg.constData();
        arg.replace("\"", "\\\"");
        mClangLine += arg;
        mClangLine += ' ';
    }

    mClangLine += mPath;

    mUnit = clang_parseTranslationUnit(mIndex, mPath.constData(),
                                       clangArgs.data(), idx, 0, 0,
                                       CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);
    warning() << "loading unit " << mClangLine << " " << (mUnit != 0);
    if (!mUnit) {
        error() << "got failure" << mClangLine;
        mData->dependencies[mFileId].insert(mFileId);
    }
}

void IndexerJob::diagnose()
{
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

        CXSourceLocation loc = clang_getDiagnosticLocation(diagnostic);
        const unsigned diagnosticOptions = (CXDiagnostic_DisplaySourceLocation|
                                            CXDiagnostic_DisplayColumn|
                                            CXDiagnostic_DisplaySourceRanges|
                                            CXDiagnostic_DisplayOption|
                                            CXDiagnostic_DisplayCategoryId|
                                            CXDiagnostic_DisplayCategoryName);

        ByteArray string;
        CXFile file;
        clang_getSpellingLocation(loc, &file, 0, 0, 0);
        if (file) {
            string = RTags::eatString(clang_formatDiagnostic(diagnostic, diagnosticOptions));
            mData->diagnostics[Location(file, 0).fileId()].append(string);
        }
        if (testLog(logLevel) || (logLevel >= Warning && testLog(CompilationError))) {
            if (string.isEmpty())
                string = RTags::eatString(clang_formatDiagnostic(diagnostic, diagnosticOptions));
            log(logLevel, "%s: %s => %s", mPath.constData(), mClangLine.constData(), string.constData());
            log(CompilationError, "%s", string.constData());
        }

        const unsigned fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
        for (unsigned f=0; f<fixItCount; ++f) {
            CXSourceRange range;
            ByteArray string = RTags::eatString(clang_getDiagnosticFixIt(diagnostic, f, &range));
            const Location start(clang_getRangeStart(range));
            unsigned endOffset = 0;
            clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &endOffset);

            error("Fixit (%d/%d) for %s: [%s] %s-%d", f + 1, fixItCount, mPath.constData(),
                  string.constData(), start.key().constData(), endOffset);
            mData->fixIts[start] = std::pair<int, ByteArray>(endOffset - start.offset(), string);
        }

        clang_disposeDiagnostic(diagnostic);
    }
    if (!hasCompilationErrors) {
        log(CompilationError, "%s parsed", mPath.constData());
    }
}

void IndexerJob::visit()
{
    if (!mUnit)
        return;
    clang_getInclusions(mUnit, inclusionVisitor, this);
    if (isAborted())
        return;

    clang_visitChildren(clang_getTranslationUnitCursor(mUnit), indexVisitor, this);
    if (isAborted())
        return;
    if (testLog(VerboseDebug)) {
        VerboseVisitorUserData u = { 0, "<VerboseVisitor " + mClangLine + ">\n", this };
        clang_visitChildren(clang_getTranslationUnitCursor(mUnit), verboseVisitor, &u);
        u.out += "</VerboseVisitor " + mClangLine + ">";
        if (getenv("RTAGS_INDEXERJOB_DUMP_TO_FILE")) {
            char buf[1024];
            snprintf(buf, sizeof(buf), "/tmp/%s.log", mPath.fileName());
            FILE *f = fopen(buf, "w");
            assert(f);
            fwrite(u.out.constData(), 1, u.out.size(), f);
            fclose(f);
        } else {
            logDirect(VerboseDebug, u.out);
        }
        // {
        //     VerboseVisitorUserData u = { -1, "<VerboseVisitor2 " + clangLine + ">", this };
        //    clang_visitChildren(clang_getTranslationUnitCursor(mUnit), verboseVisitor, &u);
        //     u.out += "</VerboseVisitor2 " + clangLine + ">";
        //     logDirect(VerboseDebug, u.out);
        // }
    }
}

void IndexerJob::run()
{
    mData.reset(new IndexData);
    if (mIndexer.lock()) {
        typedef void (IndexerJob::*Function)();
        Function functions[] = { &IndexerJob::parse, &IndexerJob::diagnose, &IndexerJob::visit };
        for (unsigned i=0; i<sizeof(functions) / sizeof(Function); ++i) {
            (this->*functions[i])();
            if (isAborted())
                break;
        }

        mHeaderMap.clear();
        char buf[1024];
        const int w = snprintf(buf, sizeof(buf), "Visited %s (%s) in %sms. (%d syms, %d refs, %d deps, %d symNames)%s",
                               mPath.constData(), mUnit ? "success" : "error", ByteArray::number(mTimer.elapsed()).constData(),
                               mData->symbols.size(), mData->references.size(), mData->dependencies.size(), mData->symbolNames.size(),
                               mFlags & Dirty ? " (dirty)" : "");
        mData->message = ByteArray(buf, w);
    } else {
        parse();
        if (mUnit) {
            DumpUserData u = { 0, this };
            clang_visitChildren(clang_getTranslationUnitCursor(mUnit), dumpVisitor, &u);
        }
    }
    if (mUnit) {
        clang_disposeTranslationUnit(mUnit);
        mUnit = 0;
    }
    if (mIndex) {
        clang_disposeIndex(mIndex);
        mIndex = 0;
    }

    if (shared_ptr<Indexer> idx = indexer())
        idx->onJobFinished(this);
}

CXChildVisitResult IndexerJob::verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData)
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
            if (u->job->mData->references.contains(loc)) {
                u->out += " used as reference\n";
            } else if (u->job->mData->symbols.contains(loc)) {
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

CXChildVisitResult IndexerJob::dumpVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    DumpUserData *dump = reinterpret_cast<DumpUserData*>(userData);
    assert(dump);
    assert(dump->job);
    Location loc = dump->job->createLocation(cursor);
    if (loc.fileId()) {
        CXCursor ref = clang_getCursorReferenced(cursor);

        ByteArray out;
        out.reserve(256);
        if (dump->indent >= 0)
            out += ByteArray(dump->indent, ' ');
        out += RTags::cursorToString(cursor, RTags::AllCursorToStringFlags);
        if (clang_equalCursors(ref, cursor)) {
            out += " refs self";
        } else if (!clang_equalCursors(ref, nullCursor)) {
            out += " refs " + RTags::cursorToString(ref, RTags::AllCursorToStringFlags);
        }
        dump->job->write(out);
    }
    if (dump->indent >= 0) {
        DumpUserData userData = { dump->indent + 2, dump->job };
        clang_visitChildren(cursor, dumpVisitor, &userData);
        return CXChildVisit_Continue;
    } else {
        return CXChildVisit_Recurse;
    }
}
