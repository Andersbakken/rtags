#include "IndexerJob.h"
#include <rct/StopWatch.h>
#include <rct/MemoryMonitor.h>
#include "Server.h"
#include <rct/EventLoop.h>
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
    String out;
    IndexerJob *job;
};

IndexerJob::IndexerJob(const shared_ptr<Project> &project, unsigned flags,
                       const SourceInformation &sourceInformation)
    : Job(0, project), mSourceInformation(sourceInformation), mFileId(Location::insertFile(sourceInformation.sourceFile)),
      mUnits(sourceInformation.builds.size()), mDump(false), mParseTime(0), mStarted(false)
{
}
IndexerJob::IndexerJob(const QueryMessage &msg, const shared_ptr<Project> &project,
                       const SourceInformation &sourceInformation)
    : Job(msg, WriteUnfiltered|WriteBuffered, project), mFlags(0), mSourceInformation(sourceInformation),
      mFileId(Location::insertFile(sourceInformation.sourceFile)), mUnits(sourceInformation.builds.size()),
      mDump(true), mParseTime(0), mStarted(false)
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
    job->mData->symbolNames[String(fn, strlen(fn))].insert(l);

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

static inline void addToSymbolNames(const String &arg, bool hasTemplates, const Location &location, SymbolNameMap &symbolNames)
{
    symbolNames[arg].insert(location);
    if (hasTemplates) {
        String copy = arg;
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

String IndexerJob::addNamePermutations(const CXCursor &cursor, const Location &location)
{
    int retLength = -1;
    String qparam, qnoparam;

    CXCursor cur = cursor;
    CXCursorKind kind;

    bool first = true;
    while (true) {
        if (clang_equalCursors(cur, nullCursor))
            break;
        kind = clang_getCursorKind(cur);

        if (first) {
            first = false;
        } else if (!RTags::needsQualifiers(kind)) {
            break;
        }

        CXStringScope displayName(clang_getCursorDisplayName(cur));
        const char *name = displayName.data();
        if (!name || !strlen(name)) {
            break;
        }
        const String qname(name);
        if (qparam.isEmpty()) {
            qparam = qname;
            if (kind == CXCursor_VarDecl || kind == CXCursor_ParmDecl) {
                retLength = qparam.size();
            }
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

        if (!RTags::needsQualifiers(kind))
            break;
        cur = clang_getCursorSemanticParent(cur);
    }

    return retLength == -1 ? qparam : qparam.right(retLength);
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

Location IndexerJob::createLocation(const CXSourceLocation &location, bool *blocked)
{
    Location ret;
    if (blocked)
        *blocked = false;
    if (!clang_equalLocations(location, nullLocation)) {
        CXFile file;
        unsigned start;
        clang_getSpellingLocation(location, &file, 0, 0, &start);
        if (file) {
            String fileName = RTags::eatString(clang_getFileName(file));
            uint32_t &fileId = mFileIds[fileName];
            if (!fileId)
                fileId = Location::insertFile(Path::resolved(fileName));
            ret = Location(fileId, start);
            if (blocked) {
                if (mVisitedFiles.contains(fileId)) {
                    *blocked = false;
                } else if (mBlockedFiles.contains(fileId)) {
                    *blocked = true;
                    ret.clear();
                } else {
                    shared_ptr<Project> p = project();
                    const bool ok = p && p->visitFile(fileId);
                    *blocked = !ok;
                    if (!ok) {
                        ret.clear();
                        mBlockedFiles.insert(fileId);
                    } else {
                        mVisitedFiles.insert(fileId);
                    }
                }
            }
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

CXChildVisitResult IndexerJob::indexVisitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
    IndexerJob *job = static_cast<IndexerJob*>(data);
    {
        MutexLocker lock(&job->mMutex);
        if (job->mAborted)
            return CXChildVisit_Break;
    }

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
        switch (kind) {
        case CXCursor_OverloadedDeclRef: {
            const int count = clang_getNumOverloadedDecls(cursor);
            for (int i=0; i<count; ++i) {
                const CXCursor ref = clang_getOverloadedDecl(cursor, i);
                job->handleReference(cursor, kind, loc, ref, parent);
            }
            break; }
        case CXCursor_CXXDeleteExpr:
            job->handleReference(cursor, kind, loc, findDestructorForDelete(cursor), parent);
            break;
        default:
            job->handleReference(cursor, kind, loc, clang_getCursorReferenced(cursor), parent);
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

void IndexerJob::superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
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
                            handleReference(cursor, kind, Location(location.fileId(), offset + 1), alternatives.first(), parent);
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


void IndexerJob::handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &location, const CXCursor &ref, const CXCursor &parent)
{
    const CXCursorKind refKind = clang_getCursorKind(ref);
    if (clang_isInvalid(refKind)) {
        superclassTemplateMemberFunctionUgleHack(cursor, kind, location, ref, parent);
        return;
    }

    bool isOperator = false;
    if (kind == CXCursor_CallExpr
        && (refKind == CXCursor_CXXMethod
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
        return;
    } else {
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
        info.definition = false;
        info.kind = kind;
        info.symbolLength = isOperator ? end - start : refInfo.symbolLength;
        info.symbolName = refInfo.symbolName;
        info.type = clang_getCursorType(cursor).kind;
        if (kind == CXCursor_TypeRef) {
            switch (clang_getCursorKind(parent)) {
            case CXCursor_FunctionDecl:
            case CXCursor_CXXMethod:
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
            case CXCursor_FieldDecl: {
                SymbolMap::iterator it = mData->symbols.find(createLocation(parent, 0));
                if (it != mData->symbols.end()) {
                    CursorInfo &ci = it->second;
                    switch (ci.type) {
                    case CXType_Pointer:
                        ci.symbolName.prepend(info.symbolName + " *");
                        break;
                    case CXType_LValueReference:
                        ci.symbolName.prepend(info.symbolName + " &");
                        break;
                    default:
                        ci.symbolName.prepend(info.symbolName + " ");
                        break;
                    }
                }
                break; }
            default:
                break;
            }

        }
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
                String include = "#include ";
                const Path path = refLoc.path();
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

static inline bool addType(String &symbolName, CXTypeKind kind)
{
    const char *type = 0;
    switch (kind) {
    case CXType_Void: type = "void "; break;
    case CXType_Bool: type = "bool "; break;
    case CXType_Char_U: type = "char_u "; break;
    case CXType_UChar: type = "unsigned char "; break;
    case CXType_Char16: type = "char16 "; break;
    case CXType_Char32: type = "char32 "; break;
    case CXType_UShort: type = "unsigned short "; break;
    case CXType_UInt: type = "unsigned int "; break;
    case CXType_ULong: type = "unsigned long "; break;
    case CXType_ULongLong: type = "unsigned long long "; break;
    case CXType_UInt128: type = "uint128 "; break;
    case CXType_Char_S: type = "char_s "; break;
    case CXType_SChar: type = "schar "; break;
    case CXType_WChar: type = "wchar "; break;
    case CXType_Short: type = "short "; break;
    case CXType_Int: type = "int "; break;
    case CXType_Long: type = "long "; break;
    case CXType_LongLong: type = "long long "; break;
    case CXType_Int128: type = "int128 "; break;
    case CXType_Float: type = "float "; break;
    case CXType_Double: type = "double "; break;
    case CXType_LongDouble: type = "long double "; break;
    default:
        return false;
    }
    assert(type);
    symbolName.prepend(type);
    return true;
}

bool IndexerJob::handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location)
{
    CursorInfo &info = mData->symbols[location];
    if (!info.symbolLength || !RTags::isCursor(info.kind)) {
        CXStringScope name = clang_getCursorSpelling(cursor);
        const char *cstr = name.data();
        info.symbolLength = cstr ? strlen(cstr) : 0;
        info.type = clang_getCursorType(cursor).kind;
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

            switch (kind) {
            case CXCursor_FunctionDecl:
            case CXCursor_CXXMethod:
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
            case CXCursor_FieldDecl:
                addType(info.symbolName, info.type);
                break;
            default:
                break;
            }
        }

        CXSourceRange range = clang_getCursorExtent(cursor);
        unsigned start, end;
        clang_getSpellingLocation(clang_getRangeStart(range), 0, 0, 0, &start);
        clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
        info.start = start;
        info.end = end;

        if (kind == CXCursor_EnumConstantDecl) {
            info.enumValue = clang_getEnumConstantDeclValue(cursor);
        } else {
            info.definition = clang_isCursorDefinition(cursor);
        }
        info.kind = kind;
        const String usr = RTags::eatString(clang_getCursorUSR(cursor));
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

bool IndexerJob::parse(int build)
{
    CXIndex &index = mUnits[build].first;
    if (!index)
        index = clang_createIndex(0, 1);
    if (!index) {
        abort();
        return false;
    }
    const List<String> args = mSourceInformation.builds.at(build).args;
    const List<String> &defaultArguments = Server::instance()->options().defaultArguments;
    CXTranslationUnit &unit = mUnits[build].second;
    assert(!unit);
    mClangLines.append(String());
    String &clangLine = mClangLines[build];

    clangLine = Server::instance()->clangPath();
    clangLine += ' ';

    int idx = 0;
    List<const char*> clangArgs(args.size() + defaultArguments.size(), 0);

    const List<String> *lists[] = { &args, &defaultArguments };
    for (int i=0; i<2; ++i) {
        const int count = lists[i]->size();
        for (int j=0; j<count; ++j) {
            String arg = lists[i]->at(j);
            if (arg.isEmpty())
                continue;

            clangArgs[idx++] = lists[i]->at(j).constData();
            arg.replace("\"", "\\\"");
            clangLine += arg;
            clangLine += ' ';
        }
    }

    clangLine += mSourceInformation.sourceFile;

    unit = clang_parseTranslationUnit(index, mSourceInformation.sourceFile.constData(),
                                      clangArgs.data(), idx, 0, 0,
                                      CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);

    warning() << "loading unit " << clangLine << " " << (unit != 0);
    if (unit) {
        return !isAborted();
    }

    error() << "got failure" << clangLine;
    const String preprocessorOnly = RTags::filterPreprocessor(mSourceInformation.sourceFile);
    if (!preprocessorOnly.isEmpty()) {
        CXUnsavedFile unsaved = { mSourceInformation.sourceFile.constData(), preprocessorOnly.constData(),
                                  static_cast<unsigned long>(preprocessorOnly.size()) };
        unit = clang_parseTranslationUnit(index, mSourceInformation.sourceFile.constData(),
                                          clangArgs.data(), idx, &unsaved, 1,
                                          CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);
    }
    if (unit) {
        clang_getInclusions(unit, IndexerJob::inclusionVisitor, this);
        clang_disposeTranslationUnit(unit);
        unit = 0;
    } else {
        mData->dependencies[mFileId].insert(mFileId);
    }
    return !isAborted();
}

bool IndexerJob::diagnose(int build, int *errorCount)
{
    if (errorCount)
        *errorCount = 0;
    if (!mUnits.at(build).second) {
        abort();
        return false;
    }

    const unsigned diagnosticCount = clang_getNumDiagnostics(mUnits.at(build).second);
    for (unsigned i=0; i<diagnosticCount; ++i) {
        CXDiagnostic diagnostic = clang_getDiagnostic(mUnits.at(build).second, i);
        int logLevel = INT_MAX;
        const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);
        switch (severity) {
        case CXDiagnostic_Fatal:
        case CXDiagnostic_Error:
            if (errorCount)
                ++*errorCount;
            logLevel = Error;
            break;
        case CXDiagnostic_Warning:
            logLevel = Warning;
            break;
        case CXDiagnostic_Note:
            logLevel = Debug;
            break;
        case CXDiagnostic_Ignored:
            break;
        }

        const unsigned diagnosticOptions = (CXDiagnostic_DisplaySourceLocation|
                                            CXDiagnostic_DisplayColumn|
                                            CXDiagnostic_DisplaySourceRanges|
                                            CXDiagnostic_DisplayOption|
                                            CXDiagnostic_DisplayCategoryId|
                                            CXDiagnostic_DisplayCategoryName);
        const uint32_t fileId = createLocation(clang_getDiagnosticLocation(diagnostic), 0).fileId();
        const String text = RTags::eatString(clang_formatDiagnostic(diagnostic, diagnosticOptions));
        if (fileId)
            mData->diagnostics[fileId].append(text);
        if (testLog(logLevel) || testLog(CompilationError)) {
            log(logLevel, "%s: %s => %s", mSourceInformation.sourceFile.constData(), mClangLines.at(build).constData(), text.constData());
            log(CompilationError, "%s", text.constData());
        }

        const unsigned fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
        RegExp rx;
        if (mFlags & IgnorePrintfFixits) {
            rx = "^%[A-Za-z0-9]\\+$";
        }
        for (unsigned f=0; f<fixItCount; ++f) {
            CXSourceRange range;
            const String string = RTags::eatString(clang_getDiagnosticFixIt(diagnostic, f, &range));
            unsigned startOffset;
            CXFile file;
            clang_getSpellingLocation(clang_getRangeStart(range), &file, 0, 0, &startOffset);
            unsigned endOffset;
            clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &endOffset);
            const Location loc(file, startOffset);
            if (mFlags & IgnorePrintfFixits && rx.indexIn(string) == 0) {
                error("Ignored fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
                      startOffset, endOffset, string.constData());
            } else {
                error("Fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
                      startOffset, endOffset, string.constData());
                log(CompilationError, "Fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
                    startOffset, endOffset, string.constData());
                mData->fixIts[loc.fileId()].insert(FixIt(startOffset, endOffset, string));
            }
        }

        clang_disposeDiagnostic(diagnostic);
    }
    if (testLog(CompilationError)) {
        log(CompilationError, "$");
    }
    return !isAborted();
}

bool IndexerJob::visit(int build)
{
    if (!mUnits.at(build).second) {
        abort();
        return false;
    }
    clang_getInclusions(mUnits.at(build).second, IndexerJob::inclusionVisitor, this);
    if (isAborted())
        return false;

    clang_visitChildren(clang_getTranslationUnitCursor(mUnits.at(build).second),
                        IndexerJob::indexVisitor, this);
    if (isAborted())
        return false;
    if (testLog(VerboseDebug)) {
        VerboseVisitorUserData u = { 0, "<VerboseVisitor " + mClangLines.at(build) + ">\n", this };
        clang_visitChildren(clang_getTranslationUnitCursor(mUnits.at(build).second),
                            IndexerJob::verboseVisitor, &u);
        u.out += "</VerboseVisitor " + mClangLines.at(build) + ">";
        if (getenv("RTAGS_INDEXERJOB_DUMP_TO_FILE")) {
            char buf[1024];
            snprintf(buf, sizeof(buf), "/tmp/%s.log", mSourceInformation.sourceFile.fileName());
            FILE *f = fopen(buf, "w");
            assert(f);
            fwrite(u.out.constData(), 1, u.out.size(), f);
            fclose(f);
        } else {
            logDirect(VerboseDebug, u.out);
        }
    }
    return !isAborted();
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
            for (int i=0; i<mSourceInformation.builds.size(); ++i) {
                parse(i);
                if (mUnits.at(i).second) {
                    DumpUserData u = { 0, this, !(queryFlags() & QueryMessage::NoContext) };
                    clang_visitChildren(clang_getTranslationUnitCursor(mUnits.at(i).second),
                                        IndexerJob::dumpVisitor, &u);
                }
            }
        }
    } else {
        {
            MutexLocker lock(&mMutex);
            mStarted = true;
        }
        int errorCount = 0;
        int unitCount = 0;
        const int buildCount = mSourceInformation.builds.size();
        for (int i=0; i<buildCount; ++i) {
            if (!parse(i)) {
                goto end;
            }
            if (mUnits.at(i).second)
                ++unitCount;
        }
        mParseTime = time(0);

        for (int i=0; i<buildCount; ++i) {
            int err = 0;
            if (!diagnose(i, &err) || !visit(i))
                goto end;
            errorCount += err;
        }
        {
            mData->message += mSourceInformation.sourceFile.toTilde();
            if (buildCount > 1)
                mData->message += String::format<16>(" (%d builds)", buildCount);
            if (!unitCount) {
                mData->message += " error";
            } else if (unitCount != buildCount) {
                mData->message += String::format<16>(" (%d errors, %d ok)", buildCount - unitCount, unitCount);
            }
            mData->message += String::format<16>(" in %sms. ", String::number(mTimer.elapsed()).constData());
            if (unitCount) {
                mData->message += String::format<1024>("(%d syms, %d symNames, %d refs, %d deps, %d files)",
                                                          mData->symbols.size(), mData->symbolNames.size(), mData->references.size(),
                                                          mData->dependencies.size(), mVisitedFiles.size());
            } else if (mData->dependencies.size()) {
                mData->message += String::format<16>("(%d deps)", mData->dependencies.size());
            }
            if (mFlags & Dirty)
                mData->message += " (dirty)";
        }
  end:
        shared_ptr<Project> p = project();
        if (p) {
            shared_ptr<IndexerJob> job = static_pointer_cast<IndexerJob>(shared_from_this());
            p->onJobFinished(job);
        }
    }
    for (int i=0; i<mUnits.size(); ++i) {
        if (mUnits.at(i).first)
            clang_disposeIndex(mUnits.at(i).first);
        if (mUnits.at(i).second)
            clang_disposeTranslationUnit(mUnits.at(i).second);
    }
    mUnits.clear();
}

CXChildVisitResult IndexerJob::verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    VerboseVisitorUserData *u = reinterpret_cast<VerboseVisitorUserData*>(userData);
    Location loc = u->job->createLocation(cursor);
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

        if (loc.fileId() && u->job->mVisitedFiles.contains(loc.fileId())) {
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
        clang_visitChildren(cursor, IndexerJob::verboseVisitor, userData);
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

        String out;
        out.reserve(256);
        int col = -1;
        if (dump->showContext) {
            out.append(loc.context(&col));
            if (col != -1) {
                out.append(String::format<32>(" // %d, %d: ", col, dump->indentLevel));
            } else {
                out.append(String::format<32>(" // %d: ", dump->indentLevel));
            }
        } else {
            out.append(String(dump->indentLevel * 2, ' '));
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
    clang_visitChildren(cursor, IndexerJob::dumpVisitor, userData);
    --dump->indentLevel;
    return CXChildVisit_Continue;
}
bool IndexerJob::abortIfStarted()
{
    MutexLocker lock(&mMutex);
    if (mStarted)
        mAborted = true;
    return mAborted;
}

