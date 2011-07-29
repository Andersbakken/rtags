#include "Utils.h"
#include <clang-c/Index.h>

#ifdef DEBUG_FUNCTION_CALLS
int Timer::s_indent = 0;
QMutex Timer::s_mutex;
#endif
bool Options::s_verbose = false;
bool Options::s_traceFunctionCalls = false;

const char *kindToString(int kind)
{
    switch (kind) {
    case CXCursor_UnexposedDecl: return "UnexposedDecl";
    case CXCursor_StructDecl: return "StructDecl";
    case CXCursor_UnionDecl: return "UnionDecl";
    case CXCursor_ClassDecl: return "ClassDecl";
    case CXCursor_EnumDecl: return "EnumDecl";
    case CXCursor_FieldDecl: return "FieldDecl";
    case CXCursor_EnumConstantDecl: return "EnumConstantDecl";
    case CXCursor_FunctionDecl: return "FunctionDecl";
    case CXCursor_VarDecl: return "VarDecl";
    case CXCursor_ParmDecl: return "ParmDecl";
    case CXCursor_ObjCInterfaceDecl: return "ObjCInterfaceDecl";
    case CXCursor_ObjCCategoryDecl: return "ObjCCategoryDecl";
    case CXCursor_ObjCProtocolDecl: return "ObjCProtocolDecl";
    case CXCursor_ObjCPropertyDecl: return "ObjCPropertyDecl";
    case CXCursor_ObjCIvarDecl: return "ObjCIvarDecl";
    case CXCursor_ObjCInstanceMethodDecl: return "ObjCInstanceMethodDecl";
    case CXCursor_ObjCClassMethodDecl: return "ObjCClassMethodDecl";
    case CXCursor_ObjCImplementationDecl: return "ObjCImplementationDecl";
    case CXCursor_ObjCCategoryImplDecl: return "ObjCCategoryImplDecl";
    case CXCursor_TypedefDecl: return "TypedefDecl";
    case CXCursor_CXXMethod: return "CXXMethod";
    case CXCursor_Namespace: return "Namespace";
    case CXCursor_LinkageSpec: return "LinkageSpec";
    case CXCursor_Constructor: return "Constructor";
    case CXCursor_Destructor: return "Destructor";
    case CXCursor_ConversionFunction: return "ConversionFunction";
    case CXCursor_TemplateTypeParameter: return "TemplateTypeParameter";
    case CXCursor_NonTypeTemplateParameter: return "NonTypeTemplateParameter";
    case CXCursor_TemplateTemplateParameter: return "TemplateTemplateParameter";
    case CXCursor_FunctionTemplate: return "FunctionTemplate";
    case CXCursor_ClassTemplate: return "ClassTemplate";
    case CXCursor_ClassTemplatePartialSpecialization: return "ClassTemplatePartialSpecialization";
    case CXCursor_NamespaceAlias: return "NamespaceAlias";
    case CXCursor_UsingDirective: return "UsingDirective";
    case CXCursor_UsingDeclaration: return "UsingDeclaration";
    case CXCursor_TypeAliasDecl: return "TypeAliasDecl";
    case CXCursor_ObjCSynthesizeDecl: return "ObjCSynthesizeDecl";
    case CXCursor_ObjCDynamicDecl: return "ObjCDynamicDecl";
    // case CXCursor_FirstDecl: return "FirstDecl";
    // case CXCursor_LastDecl: return "LastDecl";
    case CXCursor_FirstRef: return "FirstRef";
    // case CXCursor_ObjCSuperClassRef: return "ObjCSuperClassRef";
    case CXCursor_ObjCProtocolRef: return "ObjCProtocolRef";
    case CXCursor_ObjCClassRef: return "ObjCClassRef";
    case CXCursor_TypeRef: return "TypeRef";
    case CXCursor_CXXBaseSpecifier: return "CXXBaseSpecifier";
    case CXCursor_TemplateRef: return "TemplateRef";
    case CXCursor_NamespaceRef: return "NamespaceRef";
    case CXCursor_MemberRef: return "MemberRef";
    case CXCursor_LabelRef: return "LabelRef";
    case CXCursor_OverloadedDeclRef: return "OverloadedDeclRef";
    // case CXCursor_LastRef: return "LastRef";
    case CXCursor_FirstInvalid: return "FirstInvalid";
    // case CXCursor_InvalidFile: return "InvalidFile";
    case CXCursor_NoDeclFound: return "NoDeclFound";
    case CXCursor_NotImplemented: return "NotImplemented";
    case CXCursor_InvalidCode: return "InvalidCode";
    // case CXCursor_LastInvalid: return "LastInvalid";
    case CXCursor_FirstExpr: return "FirstExpr";
    // case CXCursor_UnexposedExpr: return "UnexposedExpr";
    case CXCursor_DeclRefExpr: return "DeclRefExpr";
    case CXCursor_MemberRefExpr: return "MemberRefExpr";
    case CXCursor_CallExpr: return "CallExpr";
    case CXCursor_ObjCMessageExpr: return "ObjCMessageExpr";
    case CXCursor_BlockExpr: return "BlockExpr";
    // case CXCursor_LastExpr: return "LastExpr";
    case CXCursor_FirstStmt: return "FirstStmt";
    // case CXCursor_UnexposedStmt: return "UnexposedStmt";
    case CXCursor_LabelStmt: return "LabelStmt";
    // case CXCursor_LastStmt: return "LastStmt";
    case CXCursor_TranslationUnit: return "TranslationUnit";
    case CXCursor_FirstAttr: return "FirstAttr";
    // case CXCursor_UnexposedAttr: return "UnexposedAttr";
    case CXCursor_IBActionAttr: return "IBActionAttr";
    case CXCursor_IBOutletAttr: return "IBOutletAttr";
    case CXCursor_IBOutletCollectionAttr: return "IBOutletCollectionAttr";
    // case CXCursor_LastAttr: return "LastAttr";
    case CXCursor_PreprocessingDirective: return "PreprocessingDirective";
    case CXCursor_MacroDefinition: return "MacroDefinition";
    case CXCursor_MacroExpansion: return "MacroExpansion";
    // case CXCursor_MacroInstantiation: return "MacroInstantiation";
    case CXCursor_InclusionDirective: return "InclusionDirective";
    // case CXCursor_FirstPreprocessing: return "FirstPreprocessing";
    // case CXCursor_LastPreprocessing: return "LastPreprocessing";
    }
    return "";
}

