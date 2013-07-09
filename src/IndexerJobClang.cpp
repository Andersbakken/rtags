#include "IndexerJobClang.h"
#include "Project.h"
#include "Server.h"

#include "RTagsPlugin.h"

class ClangPlugin : public RTagsPlugin
{
public:
    virtual shared_ptr<IndexerJob> createJob(const shared_ptr<Project> &project,
                                             IndexerJob::Type type,
                                             const SourceInformation &sourceInformation)
    {
        if (!sourceInformation.isJS())
            return shared_ptr<IndexerJob>(new IndexerJobClang(project, type, sourceInformation));
        return shared_ptr<IndexerJob>();
    }
    virtual shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                             const shared_ptr<Project> &project,
                                             const SourceInformation &sourceInformation)
    {
        if (!sourceInformation.isJS())
            return shared_ptr<IndexerJob>(new IndexerJobClang(msg, project, sourceInformation));
        return shared_ptr<IndexerJob>();
    }
};

extern "C" {
RTagsPlugin *createInstance()
{
    return new ClangPlugin;
}
};


static const CXSourceLocation nullLocation = clang_getNullLocation();
static const CXCursor nullCursor = clang_getNullCursor();

struct DumpUserData {
    int indentLevel;
    IndexerJobClang *job;
    bool showContext;
};

struct FindImplicitEqualsConstructorUserData {
    CXCursor &ref;
    bool &success;
};

struct VerboseVisitorUserData {
    int indent;
    String out;
    IndexerJobClang *job;
};

IndexerJobClang::IndexerJobClang(const shared_ptr<Project> &project, Type type,
                                 const SourceInformation &sourceInformation)
    : IndexerJob(project, type, sourceInformation), mLastCursor(nullCursor)
{
}

IndexerJobClang::IndexerJobClang(const QueryMessage &msg, const shared_ptr<Project> &project,
                                 const SourceInformation &sourceInformation)
    : IndexerJob(msg, project, sourceInformation), mLastCursor(nullCursor)
{
}

void IndexerJobClang::inclusionVisitor(CXFile includedFile,
                                       CXSourceLocation *includeStack,
                                       unsigned includeLen,
                                       CXClientData userData)
{
    IndexerJobClang *job = static_cast<IndexerJobClang*>(userData);
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

String IndexerJobClang::addNamePermutations(const CXCursor &cursor, const Location &location)
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
    const CXCursor &mCursor;
};

CXChildVisitResult IndexerJobClang::indexVisitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
    IndexerJobClang *job = static_cast<IndexerJobClang*>(data);
    {
        MutexLocker lock(&job->mutex());
        if (job->aborted())
            return CXChildVisit_Break;
    }

    const LastCursorUpdater updater(job->mLastCursor, cursor);

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

void IndexerJobClang::nestedClassConstructorCallUgleHack(const CXCursor &parent, CursorInfo &info,
                                                         CXCursorKind refKind, const Location &refLoc)
{
    if (refKind == CXCursor_Constructor
        && clang_getCursorKind(mLastCursor) == CXCursor_TypeRef
        && clang_getCursorKind(parent) == CXCursor_CXXFunctionalCastExpr) {
        const CXStringScope str = clang_getCursorSpelling(mLastCursor);
        int start = -1;
        const char *cstr = str.data();
        int idx = 0;
        while (cstr[idx]) {
            if (start == -1 && cstr[idx] == ' ') {
                start = idx;
            }
            ++idx;
        }
        if (start != -1) {
            // error() << "Changed symbolLength from" << info.symbolLength << "to" << (idx - start - 1) << "for dude reffing" << refLoc;
            info.symbolLength = idx - start - 1;
        }
        RTags::Filter in;
        in.kinds.insert(CXCursor_TypeRef);
        const List<CXCursor> typeRefs = RTags::children(parent, in);
        for (int i=0; i<typeRefs.size(); ++i) {
            const Location loc = createLocation(typeRefs.at(i));
            // error() << "Added" << refLoc << "to targets for" << typeRefs.at(i);
            mData->symbols[loc].targets.insert(refLoc);
        }
    }
}

void IndexerJobClang::superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
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


void IndexerJobClang::handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &location, const CXCursor &ref, const CXCursor &parent)
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
        switch (kind) {
        case CXCursor_CallExpr:
            nestedClassConstructorCallUgleHack(parent, info, refKind, reffedLoc);
            // see rtags/tests/nestedClassConstructorCallUgleHack/
            break;
        default:
            break;
        }
    }

    Set<Location> &val = mData->references[location];
    val.insert(reffedLoc);

}

void IndexerJobClang::addOverriddenCursors(const CXCursor& cursor, const Location& location, List<CursorInfo*>& infos)
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

void IndexerJobClang::handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location)
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

static String typeString(const CXType &type)
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
    ret += IndexerJobClang::typeName(clang_getTypeDeclaration(type));
    if (ret.endsWith(' '))
        ret.chop(1);
    return ret;
}

String IndexerJobClang::typeName(const CXCursor &cursor)
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
        ret = RTags::eatString(clang_getCursorSpelling(cursor));
        break;
    case CXCursor_FieldDecl:
        // ### If the return value is a template type we get an empty string here
    case CXCursor_VarDecl:
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

bool IndexerJobClang::handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location)
{
    CursorInfo &info = mData->symbols[location];
    if (!info.symbolLength || !RTags::isCursor(info.kind)) {
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
        unsigned start, end;
        clang_getSpellingLocation(clang_getRangeStart(range), 0, 0, 0, &start);
        clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
        info.start = start;
        info.end = end;

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
        const String usr = RTags::eatString(clang_getCursorUSR(cursor));
        if (!usr.isEmpty())
            mData->usrMap[usr].insert(location);

        switch (info.kind) {
        case CXCursor_FunctionDecl: {
            // apparently some function decls will give a different usr for
            // their definition and their declaration.  Using the canonical
            // cursor's usr allows us to join them. Check JSClassRelease in
            // JavaScriptCore for an example.
            const CXCursor canonical = clang_getCanonicalCursor(cursor);
            if (!clang_equalCursors(canonical, cursor)) {
                const String canonicalUsr = RTags::eatString(clang_getCursorUSR(canonical));
                if (canonicalUsr != usr && !canonicalUsr.isEmpty()) {
                    mData->usrMap[canonicalUsr].insert(location);
                }
            }
            break; }
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

bool IndexerJobClang::parse(int build)
{
    UnitList &units = data()->units;

    CXIndex &index = units[build].first;
    if (!index)
        index = clang_createIndex(0, 1);
    if (!index) {
        abort();
        return false;
    }
    const List<String> args = mSourceInformation.builds.at(build).args;
    CXTranslationUnit &unit = units[build].second;
    assert(!unit);

    mClangLines.append(String());
    String &clangLine = mClangLines.last();
    CXUnsavedFile unsaved = { mSourceInformation.sourceFile.constData(),
                              mContents.constData(),
                              static_cast<unsigned long>(mContents.size()) };

    RTags::parseTranslationUnit(mSourceInformation.sourceFile, args,
                                unit, index, clangLine,
                                mFileId, &mData->dependencies, &unsaved, 1);
    warning() << "loading unit " << clangLine << " " << (unit != 0);
    if (unit) {
        return !isAborted();
    }

    error() << "got failure" << clangLine;
    const String preprocessorOnly = RTags::filterPreprocessor(mSourceInformation.sourceFile);
    if (!preprocessorOnly.isEmpty()) {
        CXUnsavedFile preprocessorOnlyUnsaved = {
            mSourceInformation.sourceFile.constData(), preprocessorOnly.constData(),
            static_cast<unsigned long>(preprocessorOnly.size())
        };
        RTags::parseTranslationUnit(mSourceInformation.sourceFile, args,
                                    unit, index, clangLine,
                                    mFileId, &mData->dependencies, &preprocessorOnlyUnsaved, 1);
    }
    if (unit) {
        clang_getInclusions(unit, IndexerJobClang::inclusionVisitor, this);
        clang_disposeTranslationUnit(unit);
        unit = 0;
    } else if (type() != Dump) {
        mData->dependencies[mFileId].insert(mFileId);
    }
    return !isAborted();
}

struct XmlEntry
{
    enum Type { None, Warning, Error, Fixit };

    XmlEntry(Type t = None, const String& m = String(), int l = 0, int c = 0, int eo = -1)
        : type(t), message(m), line(l), column(c), endOffset(eo)
    {
    }

    Type type;
    String message;
    int line, column, endOffset;
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

bool IndexerJobClang::diagnose(int build)
{
    UnitList &units = data()->units;

    if (!units.at(build).second) {
        abort();
        return false;
    }

    List<String> compilationErrors;
    const unsigned diagnosticCount = clang_getNumDiagnostics(units.at(build).second);
    const unsigned options = Server::instance()->options().options;

    Map<uint32_t, Map<int, XmlEntry> > xmlEntries;
    const bool xmlEnabled = testLog(RTags::CompilationErrorXml);

    for (unsigned i=0; i<diagnosticCount; ++i) {
        CXDiagnostic diagnostic = clang_getDiagnostic(units.at(build).second, i);
        int logLevel = INT_MAX;
        const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);
        switch (severity) {
        case CXDiagnostic_Fatal:
        case CXDiagnostic_Error:
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

        const CXSourceLocation diagLoc = clang_getDiagnosticLocation(diagnostic);
        const Location loc = createLocation(diagLoc, 0);
        const uint32_t fileId = loc.fileId();
        if (mVisitedFiles.contains(fileId)) {
            if (severity >= CXDiagnostic_Error)
                ++mData->errors[fileId];
            const String msg = RTags::eatString(clang_getDiagnosticSpelling(diagnostic));
            if (xmlEnabled) {
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
                    if (!rangeCount) {
                        unsigned line, column;
                        clang_getSpellingLocation(diagLoc, 0, &line, &column, 0);

                        xmlEntries[fileId][loc.offset()] = XmlEntry(type, msg, line, column);
                    } else {
                        for (unsigned rangePos = 0; rangePos < rangeCount; ++rangePos) {
                            const CXSourceRange range = clang_getDiagnosticRange(diagnostic, rangePos);
                            const CXSourceLocation start = clang_getRangeStart(range);
                            const CXSourceLocation end = clang_getRangeEnd(range);

                            unsigned line, column, startOffset, endOffset;
                            clang_getSpellingLocation(start, 0, &line, &column, &startOffset);
                            clang_getSpellingLocation(end, 0, 0, 0, &endOffset);
                            if (!rangePos && !startOffset && !endOffset) {
                                // huh, range invalid? fall back to diag location
                                clang_getSpellingLocation(diagLoc, 0, &line, &column, 0);
                                xmlEntries[fileId][loc.offset()] = XmlEntry(type, msg, line, column);
                                break;
                            } else {
                                xmlEntries[fileId][startOffset] = XmlEntry(type, msg, line, column, endOffset);
                            }
                        }
                    }
                }
            }
            if (testLog(logLevel) || testLog(RTags::CompilationError)) {
                if (testLog(logLevel))
                    logDirect(logLevel, msg.constData());
                if (testLog(RTags::CompilationError))
                    logDirect(RTags::CompilationError, msg.constData());
            }

            const unsigned fixItCount = clang_getDiagnosticNumFixIts(diagnostic);
            RegExp rx;
            if (options & Server::IgnorePrintfFixits) {
                rx = "^%[A-Za-z0-9]\\+$";
            }
            for (unsigned f=0; f<fixItCount; ++f) {
                unsigned startOffset, line, column;
                CXFile file;
                CXSourceRange range;
                const CXStringScope stringScope = clang_getDiagnosticFixIt(diagnostic, f, &range);
                clang_getSpellingLocation(clang_getRangeStart(range), &file, &line, &column, &startOffset);

                const Location loc(file, startOffset);
                if (mVisitedFiles.contains(loc.fileId())) {
                    const char* string = clang_getCString(stringScope);
                    unsigned endOffset;
                    clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &endOffset);
                    if (options & Server::IgnorePrintfFixits && rx.indexIn(string) == 0) {
                        error("Ignored fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
                              startOffset, endOffset, string);
                    } else {
                        error("Fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
                              startOffset, endOffset, string);
                        if (xmlEnabled) {
                            XmlEntry& entry = xmlEntries[loc.fileId()][startOffset];
                            entry.type = XmlEntry::Fixit;
                            if (entry.message.isEmpty()) {
                                entry.message = String::format<64>("did you mean '%s'?", string);
                                entry.line = line;
                                entry.column = column;
                            }
                            entry.endOffset = endOffset;
                        }
                        if (testLog(logLevel) || testLog(RTags::CompilationError)) {
                            const String msg = String::format<128>("Fixit for %s: Replace %d-%d with [%s]", loc.path().constData(),
                                                                   startOffset, endOffset, string);
                            if (testLog(logLevel))
                                logDirect(logLevel, msg.constData());
                            if (testLog(RTags::CompilationError))
                                logDirect(RTags::CompilationError, msg.constData());
                        }
                        mData->fixIts[loc.fileId()].insert(FixIt(startOffset, endOffset, string));
                    }
                }
            }
        }

        clang_disposeDiagnostic(diagnostic);
    }
    if (xmlEnabled) {
        logDirect(RTags::CompilationErrorXml, "<?xml version=\"1.0\" encoding=\"utf-8\"?><checkstyle>");
        if (!xmlEntries.isEmpty()) {
            Map<uint32_t, Map<int, XmlEntry> >::const_iterator entry = xmlEntries.begin();
            const Map<uint32_t, Map<int, XmlEntry> >::const_iterator end = xmlEntries.end();

            const char* severities[] = { "none", "warning", "error", "fixit" };

            while (entry != end) {
                log(RTags::CompilationErrorXml, "<file name=\"%s\">", Location::path(entry->first).constData());
                const Map<int, XmlEntry>& map = entry->second;
                Map<int, XmlEntry>::const_iterator it = map.begin();
                const Map<int, XmlEntry>::const_iterator end = map.end();
                while (it != end) {
                    const XmlEntry& entry = it->second;
                    log(RTags::CompilationErrorXml, "<error line=\"%d\" column=\"%d\" startOffset=\"%d\" %sseverity=\"%s\" message=\"%s\"/>",
                        entry.line, entry.column, it->first,
                        (entry.endOffset == -1 ? "" : String::format<32>("endOffset=\"%d\" ", entry.endOffset).constData()),
                        severities[entry.type], xmlEscape(entry.message).constData());
                    ++it;
                }
                logDirect(RTags::CompilationErrorXml, "</file>");
                ++entry;
            }
        }

        for (Set<uint32_t>::const_iterator it = mVisitedFiles.begin(); it != mVisitedFiles.end(); ++it) {
            if (!xmlEntries.contains(*it)) {
                const String fn = Location::path(*it);
                log(RTags::CompilationErrorXml, "<file name=\"%s\"/>", fn.constData());
            }
        }

        logDirect(RTags::CompilationErrorXml, "</checkstyle>");
    }
    return !isAborted();
}

bool IndexerJobClang::visit(int build)
{
    UnitList &units = data()->units;

    if (!units.at(build).second) {
        abort();
        return false;
    }
    clang_getInclusions(units.at(build).second, IndexerJobClang::inclusionVisitor, this);
    if (isAborted())
        return false;

    clang_visitChildren(clang_getTranslationUnitCursor(units.at(build).second),
                        IndexerJobClang::indexVisitor, this);
    if (isAborted())
        return false;
    if (testLog(VerboseDebug)) {
        VerboseVisitorUserData u = { 0, "<VerboseVisitor " + mClangLines.at(build) + ">\n", this };
        clang_visitChildren(clang_getTranslationUnitCursor(units.at(build).second),
                            IndexerJobClang::verboseVisitor, &u);
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

void IndexerJobClang::index()
{
    UnitList &units = data()->units;
    units.resize(sourceInformation().builds.size());

    if (type() == Dump) {
        assert(id() != -1);
        for (int i=0; i<mSourceInformation.builds.size(); ++i) {
            parse(i);
            if (units.at(i).second) {
                DumpUserData u = { 0, this, !(queryFlags() & QueryMessage::NoContext) };
                clang_visitChildren(clang_getTranslationUnitCursor(units.at(i).second),
                                    IndexerJobClang::dumpVisitor, &u);
            }
        }
    } else {
        int unitCount = 0;
        const int buildCount = mSourceInformation.builds.size();
        mParseTime = time(0);
        mContents = mSourceInformation.sourceFile.readAll();
        for (int i=0; i<buildCount; ++i) {
            if (!parse(i))
                return;
            if (units.at(i).second)
                ++unitCount;
        }

        for (int i=0; i<buildCount; ++i) {
            if (!visit(i) || !diagnose(i))
                return;
        }
        {
            mData->message = mSourceInformation.sourceFile.toTilde();
            if (buildCount > 1)
                mData->message += String::format<16>(" (%d builds)", buildCount);
            if (!unitCount) {
                mData->message += " error";
            } else if (unitCount != buildCount) {
                mData->message += String::format<16>(" (%d errors, %d ok)", buildCount - unitCount, unitCount);
            }
            mData->message += String::format<16>(" in %dms. ", static_cast<int>(mTimer.elapsed()) / 1000);
            if (unitCount) {
                mData->message += String::format<128>("(%d syms, %d symNames, %d refs, %d deps, %d files)",
                                                      mData->symbols.size(), mData->symbolNames.size(), mData->references.size(),
                                                      mData->dependencies.size(), mVisitedFiles.size());
            } else if (mData->dependencies.size()) {
                mData->message += String::format<16>("(%d deps)", mData->dependencies.size());
            }
            if (type() == Dirty)
                mData->message += " (dirty)";
        }
    }
}

CXChildVisitResult IndexerJobClang::verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData)
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
        clang_visitChildren(cursor, IndexerJobClang::verboseVisitor, userData);
        u->indent -= 2;
        return CXChildVisit_Continue;
    } else {
        return CXChildVisit_Recurse;
    }
}

CXChildVisitResult IndexerJobClang::dumpVisitor(CXCursor cursor, CXCursor, CXClientData userData)
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
        out.append(" " + typeName(cursor) + " ");
        if (clang_equalCursors(ref, cursor)) {
            out.append("refs self");
        } else if (!clang_equalCursors(ref, nullCursor)) {
            out.append("refs ");
            out.append(RTags::cursorToString(ref, RTags::AllCursorToStringFlags));
        }
        dump->job->write(out);
    }
    ++dump->indentLevel;
    clang_visitChildren(cursor, IndexerJobClang::dumpVisitor, userData);
    --dump->indentLevel;
    return CXChildVisit_Continue;
}
