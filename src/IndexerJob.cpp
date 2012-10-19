#include "IndexerJob.h"
#include "Timer.h"
#include "MemoryMonitor.h"
#include "Server.h"
#include "EventLoop.h"
#include "RTagsClang.h"

struct DumpUserData {
    int indentLevel;
    IndexerJob *job;
    bool showContext;
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
      mArgs(arguments), mIndexer(indexer), mUnit(0), mIndex(0), mDump(false), mParseTime(0),
      mStarted(false)
{
}

IndexerJob::IndexerJob(const QueryMessage &msg, const shared_ptr<Project> &project,
                       const Path &input, const List<ByteArray> &arguments)
    : Job(msg, WriteUnfiltered|WriteBuffered, project), mFlags(0), mTimeStamp(0), mPath(input), mFileId(Location::insertFile(input)),
      mArgs(arguments), mUnit(0), mIndex(0), mDump(true), mParseTime(0), mStarted(false)
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

static inline bool walk(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_CXXMethod:
    case CXCursor_Constructor:
    case CXCursor_FunctionDecl:
    case CXCursor_Destructor:
    case CXCursor_VarDecl:
    case CXCursor_ParmDecl:
    case CXCursor_FieldDecl:
    case CXCursor_ClassTemplate:
    case CXCursor_Namespace:
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_EnumConstantDecl:
    case CXCursor_EnumDecl:
        return true;
    default:
        break;
    }
    return false;
}

ByteArray IndexerJob::addNamePermutations(const CXCursor &cursor, const Location &location)
{
    ByteArray qparam, qnoparam;

    CXCursor cur = cursor;
    CXCursorKind kind;

    bool first = true;
    while (true) {
        if (clang_equalCursors(cur, nullCursor))
            break;
        kind = clang_getCursorKind(cur);

        if (first) {
            first = false;
        } else if (!walk(kind)) {
            return qparam;
        }

        CXStringScope displayName(clang_getCursorDisplayName(cur));
        const char *name = clang_getCString(displayName.string);
        if (!name || !strlen(name)) {
            break;
        }
        const ByteArray qname(name);
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

        if (!walk(kind))
            return qparam;
        cur = clang_getCursorSemanticParent(cur);
    }
    return qparam;
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
                    shared_ptr<IndexerJob> job = std::static_pointer_cast<IndexerJob>(shared_from_this());
                    state = indexer && indexer->visitFile(fileId, job) ? Index : DontIndex;
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
        return CXChildVisit_Continue;
    } else if (loc.isNull()) {
        return CXChildVisit_Recurse;
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
    case RTags::Reference:
        if (kind == CXCursor_OverloadedDeclRef) {
            const int count = clang_getNumOverloadedDecls(cursor);
            for (int i=0; i<count; ++i) {
                const CXCursor ref = clang_getOverloadedDecl(cursor, i);
                job->handleReference(cursor, kind, loc, ref);
            }
        } else {
            const CXCursor ref = clang_getCursorReferenced(cursor);
            job->handleReference(cursor, kind, loc, ref);
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

void IndexerJob::handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &location, const CXCursor &ref)
{
    const CXCursorKind refKind = clang_getCursorKind(ref);
    if (clang_isInvalid(refKind))
        return;

    if (kind == CXCursor_CallExpr && (refKind == CXCursor_CXXMethod || refKind == CXCursor_ConversionFunction)) {
        // these are bullshit, for this construct:
        // foo.bar();
        // the position of the cursor is at the foo, not the bar.
        // They are not interesting for followLocation, renameSymbol or find
        // references so we toss them.
        return;
    } else if (refKind == CXCursor_Constructor && isImplicit(ref)) {
        return;
    } else if (refKind == CXCursor_CXXMethod) {
        CXStringScope scope = clang_getCursorDisplayName(ref);
        const char *data = scope.data();
        if (data && !strncmp(data, "operator", 8) && isImplicit(ref))
            return;
    }

    const Location refLoc = createLocation(ref, 0);
    if (!refLoc.isValid())
        return;

    CursorInfo &refInfo = mData->symbols[refLoc];
    if (!refInfo.symbolLength && !handleCursor(ref, refKind, refLoc))
        return;

    refInfo.references.insert(location);

    CursorInfo &info = mData->symbols[location];
    info.targets.insert(refLoc);

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
        unsigned start, end;
        clang_getSpellingLocation(clang_getRangeStart(range), 0, 0, 0, &start);
        clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
        info.start = start;
        info.end = end;
        info.isDefinition = false;
        info.kind = kind;
        switch (kind) {
        case CXCursor_MacroExpansion:
            info.symbolLength = refInfo.symbolLength;
            break;
        default:
            info.symbolLength = end - start;
            break;
        }
        info.symbolName = refInfo.symbolName;
        info.usr = RTags::eatString(clang_getCursorUSR(cursor));
        info.type = clang_getCursorType(cursor).kind;
        if (!info.usr.isEmpty())
            mData->usrMap[info.usr].insert(location);

    }
    Set<Location> &val = mData->references[location];
    val.insert(refLoc);
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
            info.targets.insert(refLoc);
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

bool IndexerJob::handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location)
{
    CursorInfo &info = mData->symbols[location];
    if (!info.symbolLength) {
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
                return false;
            }
        } else {
            info.symbolName = addNamePermutations(cursor, location);
        }
        CXSourceRange range = clang_getCursorExtent(cursor);
        unsigned start, end;
        clang_getSpellingLocation(clang_getRangeStart(range), 0, 0, 0, &start);
        clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
        info.start = start;
        info.end = end;

        info.isDefinition = clang_isCursorDefinition(cursor);
        info.kind = kind;
        info.type = clang_getCursorType(cursor).kind;
        info.usr = RTags::eatString(clang_getCursorUSR(cursor));
        if (!info.usr.isEmpty())
            mData->usrMap[info.usr].insert(location);

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

void IndexerJob::parse()
{
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
#ifdef OS_Darwin
    clangArgs[idx++] = "-I/usr/lib/c++/v1";
#endif
    const int count = mArgs.size();
    for (int i=0; i<count; ++i) {
        ByteArray arg = mArgs.at(i);
        if (arg.isEmpty())
            continue;

        clangArgs[idx++] = mArgs.at(i).constData();
        arg.replace("\"", "\\\"");
        mClangLine += arg;
        mClangLine += ' ';
    }

    mClangLine += mPath;

    const time_t now = time(0);
    mUnit = clang_parseTranslationUnit(mIndex, mPath.constData(),
                                       clangArgs.data(), idx, 0, 0,
                                       CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);
    warning() << "loading unit " << mClangLine << " " << (mUnit != 0);
    if (!mUnit) {
        error() << "got failure" << mClangLine;
        mData->dependencies[mFileId].insert(mFileId);
    } else {
        mParseTime = now;
    }
}

void IndexerJob::diagnose()
{
    if (!mUnit)
        return;
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
            hasCompilationErrors = true;
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
        if (testLog(logLevel) || testLog(CompilationError)) {
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
    }
}

void IndexerJob::execute()
{
    if (isAborted())
        return;
    mTimer.start();
    mData.reset(new IndexData);
    if (mDump) {
        assert(id() != -1);
        if (shared_ptr<Project> p = project()) {
            parse();
            if (mUnit) {
                DumpUserData u = { 0, this, !(queryFlags() & QueryMessage::NoContext) };
                clang_visitChildren(clang_getTranslationUnitCursor(mUnit), dumpVisitor, &u);
            }
        }
    } else if (mIndexer.lock()) {
        typedef void (IndexerJob::*Function)();
        Function functions[] = { &IndexerJob::parse, &IndexerJob::diagnose, &IndexerJob::visit };
        for (unsigned i=0; i<sizeof(functions) / sizeof(Function); ++i) {
            (this->*functions[i])();
            if (isAborted())
                break;
        }

        mData->message = ByteArray::snprintf<1024>("%s (%s) in %sms. (%d syms, %d symNames, %d refs, %d deps)%s",
                                                   mPath.constData(), mUnit ? "success" : "error", ByteArray::number(mTimer.elapsed()).constData(),
                                                   mData->symbols.size(), mData->symbolNames.size(), mData->references.size(), mData->dependencies.size(),
                                                   mFlags & Dirty ? " (dirty)" : "");
    }
    if (mUnit) {
        clang_disposeTranslationUnit(mUnit);
        mUnit = 0;
    }
    if (mIndex) {
        clang_disposeIndex(mIndex);
        mIndex = 0;
    }

    if (shared_ptr<Indexer> idx = indexer()) {
        shared_ptr<IndexerJob> job = std::static_pointer_cast<IndexerJob>(shared_from_this());
        idx->onJobFinished(job);
    }
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
        int col = -1;
        if (dump->showContext) {
            out.append(loc.context(&col));
            if (col != -1) {
                out.append(ByteArray::snprintf<32>(" // %d, %d: ", col, dump->indentLevel));
            } else {
                out.append(ByteArray::snprintf<32>(" // %d: ", dump->indentLevel));
            }
        } else {
            out.append(ByteArray(dump->indentLevel * 2, ' '));
        }
        out.append(RTags::cursorToString(cursor, RTags::AllCursorToStringFlags));
        if (clang_equalCursors(ref, cursor)) {
            out.append(" refs self");
        } else if (!clang_equalCursors(ref, nullCursor)) {
            out.append(" refs ");
            out.append(RTags::cursorToString(ref, RTags::AllCursorToStringFlags));
        }
        dump->job->write(out);
    }
    ++dump->indentLevel;
    clang_visitChildren(cursor, dumpVisitor, userData);
    --dump->indentLevel;
    return CXChildVisit_Continue;
}
bool IndexerJob::abortIfStarted()
{
    MutexLocker lock(&mMutex);
    if (mStarted) {
        resetProject();
        mIndexer.reset();
        return true;
    }
    return false;
}
