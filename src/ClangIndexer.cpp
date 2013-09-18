#include "ClangIndexer.h"
#include "QueryMessage.h"
#include "VisitFileMessage.h"
#include <rct/Connection.h>
#include <rct/EventLoop.h>
#include "IndexerMessage.h"

static const CXSourceLocation nullLocation = clang_getNullLocation();
static const CXCursor nullCursor = clang_getNullCursor();

struct DumpUserData {
    int indentLevel;
    ClangIndexer *indexer;
    bool showContext;
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
    : mType(Invalid), mUnit(0), mIndex(0), mLastCursor(nullCursor), mFileId(0), mParseTime(0),
      mVisitedFiles(0), mParseDuration(0), mVisitDuration(0)
{
}

ClangIndexer::~ClangIndexer()
{
    if (mUnit)
        clang_disposeTranslationUnit(mUnit);
    if (mIndex)
        clang_disposeIndex(mIndex);
}

bool ClangIndexer::connect(const Path &serverFile)
{
    return mConnection.connectToServer(serverFile, 1000);
}

bool ClangIndexer::index(Type type, const Path &sourceFile, const Path &project,
                         const List<String> &args, const String &contents)
{
    mProject = project;
    assert(mConnection.isConnected());
    mContents = (contents.isEmpty() ? sourceFile.readAll() : contents);
    assert(type != Invalid);
    assert(mType == Invalid);
    mType = type;
    mSourceFile = sourceFile;
    mArgs = args;
    const bool ret = parse() && visit() && diagnose();
    mParseTime = Rct::currentTimeMs();

    mMessage = sourceFile.toTilde();
    if (!mUnit) {
        mMessage += " error";
    }
    mMessage += String::format<16>(" in %dms. ", static_cast<int>(mTimer.elapsed()) / 1000);
    if (mUnit) {
        mMessage += String::format<128>("(%d syms, %d symNames, %d refs, %d deps, %d files)",
                                        mSymbols.size(), mSymbolNames.size(), mReferences.size(),
                                        mDependencies.size(), mVisitedFiles);
    } else if (mDependencies.size()) {
        mMessage += String::format<16>("(%d deps)", mDependencies.size());
    }
    if (mType == Dirty)
        mMessage += " (dirty)";
    String out;
    out.reserve(1024 * 1024 * 4); // ### could be smarter about this maybe
    Serializer serializer(out);
    const IndexerMessage msg(mFileId, mParseTime, std::move(mSymbols), std::move(mReferences),
                             std::move(mSymbolNames), std::move(mDependencies), std::move(mMessage),
                             std::move(mFixIts), std::move(mXmlDiagnostics), std::move(mVisited),
                             mParseDuration, mVisitDuration, std::move(mLogOutput));
    mConnection.send(msg);
    return ret;
}

Location ClangIndexer::createLocation(const Path &sourceFile, unsigned start, bool *blockedPtr)
{
    std::pair<uint32_t, bool> &file = mFilesToIds[sourceFile];

    if (!file.first) {
        bool blocked = false;
        enum { Timeout = 1000 };
        const Path resolved = sourceFile.resolved();
        const bool diff = (resolved != sourceFile);
        if (diff) {
            Map<Path, std::pair<uint32_t, bool> >::const_iterator it = mFilesToIds.find(resolved);
            if (it != mFilesToIds.end()) {
                file.first = it->second.first;
                file.second = it->second.second;
                if (blockedPtr) {
                    *blockedPtr = file.second;
                    if (file.second)
                        return Location();
                }
                return Location(file.first, start);
            }
        }
        QueryMessage msg(QueryMessage::VisitFile);
        msg.addProject(mProject);
        msg.setQuery(resolved);
        mConnection.newMessage().connect([&file, &blocked](Message *msg, Connection *conn) {
                assert(msg->messageId() == VisitFileMessage::MessageId);
                const VisitFileMessage *vm = static_cast<VisitFileMessage*>(msg);
                file = std::make_pair(vm->fileId(), !vm->visit());
                EventLoop::eventLoop()->quit();
            });

        mConnection.send(msg);
        EventLoop::eventLoop()->exec(Timeout);
        if (!file.first) {
            error() << "Error getting fileId for" << resolved;
            exit(1);
        }
        mVisited[file.first] = !file.second;
        if (file.second)
            ++mVisitedFiles;
        Location::set(resolved, file.first);
        if (diff)
            mFilesToIds[resolved] = file;
    }
    if (blockedPtr) {
        *blockedPtr = file.second;
        return *blockedPtr ? Location() : Location(file.first, start);
    }
    return Location(file.first, start);
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
                mSymbolNames[name].insert(location);
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

                    mSymbolNames[type + name].insert(location);
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

CXChildVisitResult ClangIndexer::indexVisitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
    ClangIndexer *indexer = static_cast<ClangIndexer*>(data);
    const LastCursorUpdater updater(indexer->mLastCursor, cursor);

    const CXCursorKind kind = clang_getCursorKind(cursor);
    const RTags::CursorType type = RTags::cursorType(kind);
    if (type == RTags::Other)
        return CXChildVisit_Recurse;

    bool blocked = false;
    Location loc = indexer->createLocation(cursor, &blocked);
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

    CursorInfo &refInfo = mSymbols[reffedLoc];
    if (!refInfo.symbolLength && !handleCursor(ref, refKind, reffedLoc))
        return;

    refInfo.references.insert(location);

    CursorInfo &info = mSymbols[location];
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

    if (!RTags::isCursor(info.kind) && (!info.symbolLength || info.bestTarget(mSymbols).kind == refKind)) {
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
    }

    Set<Location> &val = mReferences[location];
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

void ClangIndexer::handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location)
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
                assert(mFileId);
                mDependencies[Location::insertFile(path)].insert(mFileId);
                mSymbolNames[(include + path)].insert(location);
                mSymbolNames[(include + path.fileName())].insert(location);
            }
            CursorInfo &info = mSymbols[location];
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
    CursorInfo &info = mSymbols[location];
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
                mSymbols.remove(location);
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
        // apparently some function decls will give a different usr for
        // their definition and their declaration.  Using the canonical
        // cursor's usr allows us to join them. Check JSClassRelease in
        // JavaScriptCore for an example.
        const String usr = RTags::eatString(clang_getCursorUSR(clang_getCanonicalCursor(cursor)));
        if (!usr.isEmpty())
            mUsrMap[usr].insert(location);

        switch (info.kind) {
        case CXCursor_Constructor:
        case CXCursor_Destructor: {
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
    assert(!mUnit);
    assert(!mIndex);
    mIndex = clang_createIndex(0, 1);
    assert(mIndex);
    CXUnsavedFile unsaved = { mSourceFile.constData(), mContents.constData(), static_cast<unsigned long>(mContents.size()) };
    RTags::parseTranslationUnit(mSourceFile, mArgs, List<String>(), mUnit, mIndex, mClangLine, &unsaved, 1);

    mParseTime = mTimer.elapsed();
    warning() << "loading mUnit " << mClangLine << " " << (mUnit != 0);
    if (mUnit)
        return true;
    error() << "got failure" << mClangLine;
    const String preprocessorOnly = RTags::filterPreprocessor(mSourceFile);
    if (!preprocessorOnly.isEmpty()) {
        CXUnsavedFile preprocessorOnlyUnsaved = {
            mSourceFile.constData(), preprocessorOnly.constData(),
            static_cast<unsigned long>(preprocessorOnly.size())
        };
        RTags::parseTranslationUnit(mSourceFile, mArgs, List<String>(),
                                    mUnit, mIndex, mClangLine,
                                    &preprocessorOnlyUnsaved, 1);
    }
    if (mUnit) {
        clang_getInclusions(mUnit, ClangIndexer::inclusionVisitor, this);
        clang_disposeTranslationUnit(mUnit);
        mUnit = 0;
    } else if (mType != Dump) {
        bool blocked;
        mFileId = createLocation(mSourceFile, 0, &blocked).fileId();
        if (mFileId)
            mDependencies[mFileId].insert(mFileId);
    }
    return false;
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

bool ClangIndexer::diagnose()
{
    if (!mUnit) {
        return false;
    } else if (mType == Dump) {
        return true;
    }

    List<String> compilationErrors;
    const unsigned diagnosticCount = clang_getNumDiagnostics(mUnit);

    Map<uint32_t, Map<int, XmlEntry> > xmlEntries;
    const bool xmlEnabled = testLog(RTags::CompilationErrorXml);

    for (unsigned i=0; i<diagnosticCount; ++i) {
        CXDiagnostic diagnostic = clang_getDiagnostic(mUnit, i);
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
        if (mVisited.value(fileId)) {
            if (severity >= CXDiagnostic_Error)
                ++mErrors[fileId];
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
            for (unsigned f=0; f<fixItCount; ++f) {
                unsigned startOffset, line, column;
                CXFile file;
                CXSourceRange range;
                const CXStringScope stringScope = clang_getDiagnosticFixIt(diagnostic, f, &range);
                clang_getSpellingLocation(clang_getRangeStart(range), &file, &line, &column, &startOffset);

                const Location loc(file, startOffset);
                if (mVisited.value(loc.fileId())) {
                    const char* string = clang_getCString(stringScope);
                    unsigned endOffset;
                    clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &endOffset);
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
                    }
                    mFixIts[loc.fileId()].insert(FixIt(startOffset, endOffset, string));
                }
            }
        }

        clang_disposeDiagnostic(diagnostic);
    }
    mXmlDiagnostics = "<?xml version=\"1.0\" encoding=\"utf-8\"?><checkstyle>";
    if (!xmlEntries.isEmpty()) {
        Map<uint32_t, Map<int, XmlEntry> >::const_iterator entry = xmlEntries.begin();
        const Map<uint32_t, Map<int, XmlEntry> >::const_iterator end = xmlEntries.end();

        const char* severities[] = { "none", "warning", "error", "fixit" };

        while (entry != end) {
            mXmlDiagnostics += String::format<128>("<file name=\"%s\">", Location::path(entry->first).constData());
            const Map<int, XmlEntry>& map = entry->second;
            Map<int, XmlEntry>::const_iterator it = map.begin();
            const Map<int, XmlEntry>::const_iterator end = map.end();
            while (it != end) {
                const XmlEntry& entry = it->second;
                mXmlDiagnostics += String::format("<error line=\"%d\" column=\"%d\" startOffset=\"%d\" %sseverity=\"%s\" message=\"%s\"/>",
                                                  entry.line, entry.column, it->first,
                                                  (entry.endOffset == -1 ? "" : String::format<32>("endOffset=\"%d\" ", entry.endOffset).constData()),
                                                  severities[entry.type], xmlEscape(entry.message).constData());
                ++it;
            }
            mXmlDiagnostics += "</file>";
            ++entry;
        }
    }

    for (Map<uint32_t, bool>::const_iterator it = mVisited.begin(); it != mVisited.end(); ++it) {
        if (it->second && !xmlEntries.contains(it->first)) {
            const String fn = Location::path(it->first);
            mXmlDiagnostics += String::format("<file name=\"%s\"/>", fn.constData());
        }
    }

    mXmlDiagnostics += "</checkstyle>";
    return true;
}

bool ClangIndexer::visit()
{
    if (!mUnit || !mFileId) {
        return false;
    }
    StopWatch watch;

    clang_visitChildren(clang_getTranslationUnitCursor(mUnit),
                        ClangIndexer::indexVisitor, this);

    assert(mFileId);
    for (Map<uint32_t, bool>::const_iterator it = mVisited.begin(); it != mVisited.end(); ++it) {
        mDependencies[it->first].insert(mFileId);
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
            snprintf(buf, sizeof(buf), "/tmp/%s.log", mSourceFile.constData());
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

        if (loc.fileId() && u->indexer->mVisited.value(loc.fileId())) {
            if (u->indexer->mReferences.contains(loc)) {
                u->out += " used as reference\n";
            } else if (u->indexer->mSymbols.contains(loc)) {
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
        dump->indexer->mLogOutput += out;
    }
    ++dump->indentLevel;
    clang_visitChildren(cursor, ClangIndexer::dumpVisitor, userData);
    --dump->indentLevel;
    return CXChildVisit_Continue;
}

void ClangIndexer::addFileSymbol(uint32_t file)
{
    const Location loc(file, 0);
    const Path path = Location::path(file);
    mSymbolNames[path].insert(loc);
    const char *fn = path.fileName();
    mSymbolNames[String(fn, strlen(fn))].insert(loc);
}


void ClangIndexer::inclusionVisitor(CXFile includedFile,
                                    CXSourceLocation *includeStack,
                                    unsigned includeLen,
                                    CXClientData userData)
{
    ClangIndexer *indexer = static_cast<ClangIndexer*>(userData);
    const Location l(includedFile, 0);

    const uint32_t fileId = l.fileId();
    if (!includeLen) {
        indexer->mDependencies[fileId].insert(fileId);
    } else {
        for (unsigned i=0; i<includeLen; ++i) {
            CXFile originatingFile;
            clang_getSpellingLocation(includeStack[i], &originatingFile, 0, 0, 0);
            Location loc(originatingFile, 0);
            const uint32_t f = loc.fileId();
            if (f)
                indexer->mDependencies[fileId].insert(f);
        }
    }
}
