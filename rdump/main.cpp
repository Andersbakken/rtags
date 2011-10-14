#include <clang-c/Index.h>

const char *kindToString(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_UnexposedDecl: return "CXCursor_UnexposedDecl";
    case CXCursor_StructDecl: return "CXCursor_StructDecl";
    case CXCursor_UnionDecl: return "CXCursor_UnionDecl";
    case CXCursor_ClassDecl: return "CXCursor_ClassDecl";
    case CXCursor_EnumDecl: return "CXCursor_EnumDecl";
    case CXCursor_FieldDecl: return "CXCursor_FieldDecl";
    case CXCursor_EnumConstantDecl: return "CXCursor_EnumConstantDecl";
    case CXCursor_FunctionDecl: return "CXCursor_FunctionDecl";
    case CXCursor_VarDecl: return "CXCursor_VarDecl";
    case CXCursor_ParmDecl: return "CXCursor_ParmDecl";
    case CXCursor_ObjCInterfaceDecl: return "CXCursor_ObjCInterfaceDecl";
    case CXCursor_ObjCCategoryDecl: return "CXCursor_ObjCCategoryDecl";
    case CXCursor_ObjCProtocolDecl: return "CXCursor_ObjCProtocolDecl";
    case CXCursor_ObjCPropertyDecl: return "CXCursor_ObjCPropertyDecl";
    case CXCursor_ObjCIvarDecl: return "CXCursor_ObjCIvarDecl";
    case CXCursor_ObjCInstanceMethodDecl: return "CXCursor_ObjCInstanceMethodDecl";
    case CXCursor_ObjCClassMethodDecl: return "CXCursor_ObjCClassMethodDecl";
    case CXCursor_ObjCImplementationDecl: return "CXCursor_ObjCImplementationDecl";
    case CXCursor_ObjCCategoryImplDecl: return "CXCursor_ObjCCategoryImplDecl";
    case CXCursor_TypedefDecl: return "CXCursor_TypedefDecl";
    case CXCursor_CXXMethod: return "CXCursor_CXXMethod";
    case CXCursor_Namespace: return "CXCursor_Namespace";
    case CXCursor_LinkageSpec: return "CXCursor_LinkageSpec";
    case CXCursor_Constructor: return "CXCursor_Constructor";
    case CXCursor_Destructor: return "CXCursor_Destructor";
    case CXCursor_ConversionFunction: return "CXCursor_ConversionFunction";
    case CXCursor_TemplateTypeParameter: return "CXCursor_TemplateTypeParameter";
    case CXCursor_NonTypeTemplateParameter: return "CXCursor_NonTypeTemplateParameter";
    case CXCursor_TemplateTemplateParameter: return "CXCursor_TemplateTemplateParameter";
    case CXCursor_FunctionTemplate: return "CXCursor_FunctionTemplate";
    case CXCursor_ClassTemplate: return "CXCursor_ClassTemplate";
    case CXCursor_ClassTemplatePartialSpecialization: return "CXCursor_ClassTemplatePartialSpecialization";
    case CXCursor_NamespaceAlias: return "CXCursor_NamespaceAlias";
    case CXCursor_UsingDirective: return "CXCursor_UsingDirective";
    case CXCursor_UsingDeclaration: return "CXCursor_UsingDeclaration";
    case CXCursor_TypeAliasDecl: return "CXCursor_TypeAliasDecl";
    case CXCursor_ObjCSynthesizeDecl: return "CXCursor_ObjCSynthesizeDecl";
    case CXCursor_ObjCDynamicDecl: return "CXCursor_ObjCDynamicDecl";
        // case CXCursor_FirstDecl: return "CXCursor_FirstDecl";
    // case CXCursor_LastDecl: return "CXCursor_LastDecl";
    case CXCursor_FirstRef: return "CXCursor_FirstRef";
        // case CXCursor_ObjCSuperClassRef: return "CXCursor_ObjCSuperClassRef";
    case CXCursor_ObjCProtocolRef: return "CXCursor_ObjCProtocolRef";
    case CXCursor_ObjCClassRef: return "CXCursor_ObjCClassRef";
    case CXCursor_TypeRef: return "CXCursor_TypeRef";
    case CXCursor_CXXBaseSpecifier: return "CXCursor_CXXBaseSpecifier";
    case CXCursor_TemplateRef: return "CXCursor_TemplateRef";
    case CXCursor_NamespaceRef: return "CXCursor_NamespaceRef";
    case CXCursor_MemberRef: return "CXCursor_MemberRef";
    case CXCursor_LabelRef: return "CXCursor_LabelRef";
    case CXCursor_OverloadedDeclRef: return "CXCursor_OverloadedDeclRef";
        // case CXCursor_LastRef: return "CXCursor_LastRef";
    case CXCursor_FirstInvalid: return "CXCursor_FirstInvalid";
        // case CXCursor_InvalidFile: return "CXCursor_InvalidFile";
    case CXCursor_NoDeclFound: return "CXCursor_NoDeclFound";
    case CXCursor_NotImplemented: return "CXCursor_NotImplemented";
    case CXCursor_InvalidCode: return "CXCursor_InvalidCode";
        // case CXCursor_LastInvalid: return "CXCursor_LastInvalid";
    case CXCursor_FirstExpr: return "CXCursor_FirstExpr";
        // case CXCursor_UnexposedExpr: return "CXCursor_UnexposedExpr";
    case CXCursor_DeclRefExpr: return "CXCursor_DeclRefExpr";
    case CXCursor_MemberRefExpr: return "CXCursor_MemberRefExpr";
    case CXCursor_CallExpr: return "CXCursor_CallExpr";
    case CXCursor_ObjCMessageExpr: return "CXCursor_ObjCMessageExpr";
    case CXCursor_BlockExpr: return "CXCursor_BlockExpr";
        // case CXCursor_LastExpr: return "CXCursor_LastExpr";
    case CXCursor_FirstStmt: return "CXCursor_FirstStmt";
        // case CXCursor_UnexposedStmt: return "CXCursor_UnexposedStmt";
    case CXCursor_LabelStmt: return "CXCursor_LabelStmt";
        // case CXCursor_LastStmt: return "CXCursor_LastStmt";
    case CXCursor_TranslationUnit: return "CXCursor_TranslationUnit";
    case CXCursor_FirstAttr: return "CXCursor_FirstAttr";
        // case CXCursor_UnexposedAttr: return "CXCursor_UnexposedAttr";
    case CXCursor_IBActionAttr: return "CXCursor_IBActionAttr";
    case CXCursor_IBOutletAttr: return "CXCursor_IBOutletAttr";
    case CXCursor_IBOutletCollectionAttr: return "CXCursor_IBOutletCollectionAttr";
        // case CXCursor_LastAttr: return "CXCursor_LastAttr";
    case CXCursor_PreprocessingDirective: return "CXCursor_PreprocessingDirective";
    case CXCursor_MacroDefinition: return "CXCursor_MacroDefinition";
    case CXCursor_MacroExpansion: return "CXCursor_MacroExpansion";
    // case CXCursor_MacroInstantiation: return "CXCursor_MacroInstantiation";
    case CXCursor_InclusionDirective: return "CXCursor_InclusionDirective";
    case CXCursor_CXXFinalAttr: return "CXCursor_CXXFinalAttr";
    case CXCursor_CXXOverrideAttr: return "CXCursor_CXXOverrideAttr";
        // case CXCursor_FirstPreprocessing: return "CXCursor_FirstPreprocessing";
        // case CXCursor_LastPreprocessing: return "CXCursor_LastPreprocessing";
    }
    return "";
}

void printCursor(CXCursor cursor)
{
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    CXFile file;
    unsigned line, column, offset;
    clang_getInstantiationLocation(loc, &file, &line, &column, &offset);
    CXString fileName = clang_getFileName(file);
    // const char *fileNameStr = clang_getCString(fileName);
    CXString symbolName = clang_getCursorDisplayName(cursor);
    const bool isDef = clang_isCursorDefinition(cursor);
    printf("%s %s ", clang_getCString(symbolName), kindToString(clang_getCursorKind(cursor)));
    clang_disposeString(symbolName);
    if (file) {
        printf("%s:%d:%d", clang_getCString(fileName), line, column);
        clang_disposeString(fileName);
    } else {
        printf("(no file)");
    }
    if (isDef)
        printf("(def)");
}

static CXChildVisitResult dumpTree(CXCursor cursor, CXCursor, CXClientData)
{
    for (CXCursor p=clang_getCursorSemanticParent(cursor); !clang_isInvalid(clang_getCursorKind(cursor)); p = clang_getCursorSemanticParent(p)) {
        printf("  ");
    }
    printCursor(cursor);
    printf("\n");
    return CXChildVisit_Recurse;
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        printf("%s %d: if (argc < 2)\n", __FILE__, __LINE__);
        return 1;
    }
    CXIndex index = clang_createIndex(1, 0);

    CXTranslationUnit unit = clang_parseTranslationUnit(index, argv[argc - 1],
                                                        &argv[1], argc - 2, 0, 0,
                                                        // CXTranslationUnit_NestedMacroExpansions
                                                        CXTranslationUnit_DetailedPreprocessingRecord);
    if (!unit) {
        printf("Can't parse %s ", argv[1]);
        for (int i=1; i<argc - 1; ++i) {
            printf(" \"%s\"", argv[i]);
        }
        printf("\n");
    } else {
        clang_visitChildren(clang_getTranslationUnitCursor(unit), dumpTree, 0);
        fflush(stdout);
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(index);
    return 0;
}
