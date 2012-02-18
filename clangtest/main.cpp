#include <clang-c/Index.h>
#include <string.h>

static inline const char *kindToString(CXIdxEntityKind kind)
{
    switch (kind) {
    case CXIdxEntity_Unexposed: return "Unexposed";
    case CXIdxEntity_Typedef: return "Typedef";
    case CXIdxEntity_Function: return "Function";
    case CXIdxEntity_Variable: return "Variable";
    case CXIdxEntity_Field: return "Field";
    case CXIdxEntity_EnumConstant: return "EnumConstant";
    case CXIdxEntity_ObjCClass: return "ObjCClass";
    case CXIdxEntity_ObjCProtocol: return "ObjCProtocol";
    case CXIdxEntity_ObjCCategory: return "ObjCCategory";
    case CXIdxEntity_ObjCInstanceMethod: return "ObjCInstanceMethod";
    case CXIdxEntity_ObjCClassMethod: return "ObjCClassMethod";
    case CXIdxEntity_ObjCProperty: return "ObjCProperty";
    case CXIdxEntity_ObjCIvar: return "ObjCIvar";
    case CXIdxEntity_Enum: return "Enum";
    case CXIdxEntity_Struct: return "Struct";
    case CXIdxEntity_Union: return "Union";
    case CXIdxEntity_CXXClass: return "CXXClass";
    case CXIdxEntity_CXXNamespace: return "CXXNamespace";
    case CXIdxEntity_CXXNamespaceAlias: return "CXXNamespaceAlias";
    case CXIdxEntity_CXXStaticVariable: return "CXXStaticVariable";
    case CXIdxEntity_CXXStaticMethod: return "CXXStaticMethod";
    case CXIdxEntity_CXXInstanceMethod: return "CXXInstanceMethod";
    case CXIdxEntity_CXXConstructor: return "CXXConstructor";
    case CXIdxEntity_CXXDestructor: return "CXXDestructor";
    case CXIdxEntity_CXXConversionFunction: return "CXXConversionFunction";
    case CXIdxEntity_CXXTypeAlias: return "CXXTypeAlias";
    }
    return "";
}

class String
{
    String(const String &other);
    String &operator=(const String &other);
public:
    String(CXString s)
        : str(s)
    {}

    ~String()
    {
        clang_disposeString(str);
    }
    const char *data() const
    {
        return clang_getCString(str);
    }

    CXString str;
};

static CXChildVisitResult visitAll(CXCursor cursor, CXCursor, CXClientData)
{
    CXFile f;
    unsigned l, c;
    clang_getInstantiationLocation(clang_getCursorLocation(cursor), &f, &l, &c, 0);

    printf("[%s] [%s] %s:%u:%u:\n",
           String(clang_getCursorKindSpelling(clang_getCursorKind(cursor))).data(),
           String(clang_getCursorSpelling(cursor)).data(),
           String(clang_getFileName(f)).data(),
           l, c);
    return CXChildVisit_Recurse;
}

static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData)
{
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_ParmDecl:
        return CXChildVisit_Recurse;
    case CXCursor_TypeRef:
    case CXCursor_TemplateRef: {
        CXFile file;
        unsigned line, col;
        clang_getInstantiationLocation(clang_getCursorLocation(cursor), &file, &line, &col, 0);

        CXCursor ref = clang_getCursorReferenced(cursor);
        CXFile file2;
        unsigned line2, col2;
        clang_getInstantiationLocation(clang_getCursorLocation(ref), &file2, &line2, &col2, 0);
        printf("    %s %s %s:%u:%u: references %s %s %s:%u:%u:\n",
               String(clang_getCursorKindSpelling(clang_getCursorKind(cursor))).data(),
               String(clang_getCursorSpelling(cursor)).data(),
               String(clang_getFileName(file)).data(),
               line, col,
               String(clang_getCursorKindSpelling(clang_getCursorKind(ref))).data(),
               String(clang_getCursorSpelling(ref)).data(),
               String(clang_getFileName(file2)).data(),
               line2, col2);
        break; }
    default:
        break;
    }
    return CXChildVisit_Continue;
}

void indexDeclaration(CXClientData, const CXIdxDeclInfo *decl)
{
    CXFile f;
    unsigned l, c;
    clang_indexLoc_getFileLocation(decl->loc, 0, &f, &l, &c, 0);
    printf("%s:%d:%d: %s %s\n",
           String(clang_getFileName(f)).data(),
           l, c, kindToString(decl->entityInfo->kind), decl->entityInfo->name);
    switch (decl->entityInfo->kind) {
    case CXIdxEntity_Field:
    case CXIdxEntity_Variable:
    case CXIdxEntity_Function:
    case CXIdxEntity_CXXInstanceMethod:
    case CXIdxEntity_CXXConstructor:
        clang_visitChildren(decl->cursor, visitor, 0);
        break;
    default:
        break;
    }
}

void indexEntityReference(CXClientData, const CXIdxEntityRefInfo *ref)
{
    CXFile f;
    unsigned l, c;
    clang_indexLoc_getFileLocation(ref->loc, 0, &f, &l, &c, 0);

    CXSourceLocation loc = clang_getCursorLocation(ref->referencedEntity->cursor);
    CXFile f2;
    unsigned l2, c2;
    clang_getInstantiationLocation(loc, &f2, &l2, &c2, 0);
    printf("%s:%d:%d: ref of %s %s %s:%d:%d\n",
           String(clang_getFileName(f)).data(), l, c,
           kindToString(ref->referencedEntity->kind),
           ref->referencedEntity->name,
           String(clang_getFileName(f2)).data(), l2, c2);
}


int main(int argc, char **argv)
{
    CXIndex index = clang_createIndex(1, 1);
    CXIndexAction action = clang_IndexAction_create(index);
    const char *args[] = { "-cc1", "-I.", "-x", "c++" };
    IndexerCallbacks cb;
    memset(&cb, 0, sizeof(IndexerCallbacks));
    cb.indexDeclaration = indexDeclaration;
    cb.indexEntityReference = indexEntityReference;
    CXTranslationUnit unit = 0;
    clang_indexSourceFile(action, 0, &cb, sizeof(IndexerCallbacks),
                          CXIndexOpt_IndexFunctionLocalSymbols,
                          argc < 2 ? "test.cpp" : argv[1],
                          args, sizeof(args) / 4,
                          0, 0, &unit, clang_defaultEditingTranslationUnitOptions());
    clang_visitChildren(clang_getTranslationUnitCursor(unit), visitAll, 0);
    if (unit)
        clang_disposeTranslationUnit(unit);
    clang_IndexAction_dispose(action);
    clang_disposeIndex(index);
    return 0;
}
