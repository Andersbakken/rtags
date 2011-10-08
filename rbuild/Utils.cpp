#include "Utils.h"
#include <clang-c/Index.h>
#include "Path.h"

#ifdef OLDASS_CLANG
enum { CXCursor_CXXAccessSpecifier,
       CXCursor_IntegerLiteral,
       CXCursor_FloatingLiteral,
       CXCursor_ImaginaryLiteral,
       CXCursor_StringLiteral,
       CXCursor_CharacterLiteral,
       CXCursor_ParenExpr,
       CXCursor_UnaryOperator,
       CXCursor_ArraySubscriptExpr,
       CXCursor_BinaryOperator,
       CXCursor_CompoundAssignOperator,
       CXCursor_ConditionalOperator,
       CXCursor_CStyleCastExpr,
       CXCursor_CompoundLiteralExpr,
       CXCursor_InitListExpr,
       CXCursor_AddrLabelExpr,
       CXCursor_StmtExpr,
       CXCursor_GenericSelectionExpr,
       CXCursor_GNUNullExpr,
       CXCursor_CXXStaticCastExpr,
       CXCursor_CXXDynamicCastExpr,
       CXCursor_CXXReinterpretCastExpr,
       CXCursor_CXXConstCastExpr,
       CXCursor_CXXFunctionalCastExpr,
       CXCursor_CXXTypeidExpr,
       CXCursor_CXXBoolLiteralExpr,
       CXCursor_CXXNullPtrLiteralExpr,
       CXCursor_CXXThisExpr,
       CXCursor_CXXThrowExpr,
       CXCursor_CXXNewExpr,
       CXCursor_CXXDeleteExpr,
       CXCursor_UnaryExpr,
       CXCursor_ObjCStringLiteral,
       CXCursor_ObjCEncodeExpr,
       CXCursor_ObjCSelectorExpr,
       CXCursor_ObjCProtocolExpr,
       CXCursor_ObjCBridgedCastExpr,
       CXCursor_PackExpansionExpr,
       CXCursor_SizeOfPackExpr,
       CXCursor_LastExpr,
       CXCursor_CompoundStmt,
       CXCursor_CaseStmt,
       CXCursor_DefaultStmt,
       CXCursor_IfStmt,
       CXCursor_SwitchStmt,
       CXCursor_WhileStmt,
       CXCursor_DoStmt,
       CXCursor_ForStmt,
       CXCursor_GotoStmt,
       CXCursor_IndirectGotoStmt,
       CXCursor_ContinueStmt,
       CXCursor_BreakStmt,
       CXCursor_ReturnStmt,
       CXCursor_AsmStmt,
       CXCursor_ObjCAtTryStmt,
       CXCursor_ObjCAtCatchStmt,
       CXCursor_ObjCAtFinallyStmt,
       CXCursor_ObjCAtThrowStmt,
       CXCursor_ObjCAtSynchronizedStmt,
       CXCursor_ObjCAutoreleasePoolStmt,
       CXCursor_ObjCForCollectionStmt,
       CXCursor_CXXCatchStmt,
       CXCursor_CXXTryStmt,
       CXCursor_CXXForRangeStmt,
       CXCursor_SEHTryStmt,
       CXCursor_SEHExceptStmt,
       CXCursor_SEHFinallyStmt,
       CXCursor_NullStmt,
       CXCursor_DeclStmt,
       CXCursor_LastStmt
};
#endif
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
    case CXCursor_CXXAccessSpecifier: return "CXCursor_CXXAccessSpecifier";
    case CXCursor_IntegerLiteral: return "CXCursor_IntegerLiteral";
    case CXCursor_FloatingLiteral: return "CXCursor_FloatingLiteral";
    case CXCursor_ImaginaryLiteral: return "CXCursor_ImaginaryLiteral";
    case CXCursor_StringLiteral: return "CXCursor_StringLiteral";
    case CXCursor_CharacterLiteral: return "CXCursor_CharacterLiteral";
    case CXCursor_ParenExpr: return "CXCursor_ParenExpr";
    case CXCursor_UnaryOperator: return "CXCursor_UnaryOperator";
    case CXCursor_ArraySubscriptExpr: return "CXCursor_ArraySubscriptExpr";
    case CXCursor_BinaryOperator: return "CXCursor_BinaryOperator";
    case CXCursor_CompoundAssignOperator: return "CXCursor_CompoundAssignOperator";
    case CXCursor_ConditionalOperator: return "CXCursor_ConditionalOperator";
    case CXCursor_CStyleCastExpr: return "CXCursor_CStyleCastExpr";
    case CXCursor_CompoundLiteralExpr: return "CXCursor_CompoundLiteralExpr";
    case CXCursor_InitListExpr: return "CXCursor_InitListExpr";
    case CXCursor_AddrLabelExpr: return "CXCursor_AddrLabelExpr";
    case CXCursor_StmtExpr: return "CXCursor_StmtExpr";
    case CXCursor_GenericSelectionExpr: return "CXCursor_GenericSelectionExpr";
    case CXCursor_GNUNullExpr: return "CXCursor_GNUNullExpr";
    case CXCursor_CXXStaticCastExpr: return "CXCursor_CXXStaticCastExpr";
    case CXCursor_CXXDynamicCastExpr: return "CXCursor_CXXDynamicCastExpr";
    case CXCursor_CXXReinterpretCastExpr: return "CXCursor_CXXReinterpretCastExpr";
    case CXCursor_CXXConstCastExpr: return "CXCursor_CXXConstCastExpr";
    case CXCursor_CXXFunctionalCastExpr: return "CXCursor_CXXFunctionalCastExpr";
    case CXCursor_CXXTypeidExpr: return "CXCursor_CXXTypeidExpr";
    case CXCursor_CXXBoolLiteralExpr: return "CXCursor_CXXBoolLiteralExpr";
    case CXCursor_CXXNullPtrLiteralExpr: return "CXCursor_CXXNullPtrLiteralExpr";
    case CXCursor_CXXThisExpr: return "CXCursor_CXXThisExpr";
    case CXCursor_CXXThrowExpr: return "CXCursor_CXXThrowExpr";
    case CXCursor_CXXNewExpr: return "CXCursor_CXXNewExpr";
    case CXCursor_CXXDeleteExpr: return "CXCursor_CXXDeleteExpr";
    case CXCursor_UnaryExpr: return "CXCursor_UnaryExpr";
    case CXCursor_ObjCStringLiteral: return "CXCursor_ObjCStringLiteral";
    case CXCursor_ObjCEncodeExpr: return "CXCursor_ObjCEncodeExpr";
    case CXCursor_ObjCSelectorExpr: return "CXCursor_ObjCSelectorExpr";
    case CXCursor_ObjCProtocolExpr: return "CXCursor_ObjCProtocolExpr";
    case CXCursor_ObjCBridgedCastExpr: return "CXCursor_ObjCBridgedCastExpr";
    case CXCursor_PackExpansionExpr: return "CXCursor_PackExpansionExpr";
    case CXCursor_SizeOfPackExpr: return "CXCursor_SizeOfPackExpr";
    case CXCursor_CompoundStmt: return "CXCursor_CompoundStmt";
    case CXCursor_CaseStmt: return "CXCursor_CaseStmt";
    case CXCursor_DefaultStmt: return "CXCursor_DefaultStmt";
    case CXCursor_IfStmt: return "CXCursor_IfStmt";
    case CXCursor_SwitchStmt: return "CXCursor_SwitchStmt";
    case CXCursor_WhileStmt: return "CXCursor_WhileStmt";
    case CXCursor_DoStmt: return "CXCursor_DoStmt";
    case CXCursor_ForStmt: return "CXCursor_ForStmt";
    case CXCursor_GotoStmt: return "CXCursor_GotoStmt";
    case CXCursor_IndirectGotoStmt: return "CXCursor_IndirectGotoStmt";
    case CXCursor_ContinueStmt: return "CXCursor_ContinueStmt";
    case CXCursor_BreakStmt: return "CXCursor_BreakStmt";
    case CXCursor_ReturnStmt: return "CXCursor_ReturnStmt";
    case CXCursor_AsmStmt: return "CXCursor_AsmStmt";
    case CXCursor_ObjCAtTryStmt: return "CXCursor_ObjCAtTryStmt";
    case CXCursor_ObjCAtCatchStmt: return "CXCursor_ObjCAtCatchStmt";
    case CXCursor_ObjCAtFinallyStmt: return "CXCursor_ObjCAtFinallyStmt";
    case CXCursor_ObjCAtThrowStmt: return "CXCursor_ObjCAtThrowStmt";
    case CXCursor_ObjCAtSynchronizedStmt: return "CXCursor_ObjCAtSynchronizedStmt";
    case CXCursor_ObjCAutoreleasePoolStmt: return "CXCursor_ObjCAutoreleasePoolStmt";
    case CXCursor_ObjCForCollectionStmt: return "CXCursor_ObjCForCollectionStmt";
    case CXCursor_CXXCatchStmt: return "CXCursor_CXXCatchStmt";
    case CXCursor_CXXTryStmt: return "CXCursor_CXXTryStmt";
    case CXCursor_CXXForRangeStmt: return "CXCursor_CXXForRangeStmt";
    case CXCursor_SEHTryStmt: return "CXCursor_SEHTryStmt";
    case CXCursor_SEHExceptStmt: return "CXCursor_SEHExceptStmt";
    case CXCursor_SEHFinallyStmt: return "CXCursor_SEHFinallyStmt";
    case CXCursor_NullStmt: return "CXCursor_NullStmt";
    case CXCursor_DeclStmt: return "CXCursor_DeclStmt";
    }
    return "";
}


const char *completionChunkKindToString(int kind)
{
    switch (kind) {
    case CXCompletionChunk_Optional: return "Optional";
    case CXCompletionChunk_TypedText: return "TypedText";
    case CXCompletionChunk_Text: return "Text";
    case CXCompletionChunk_Placeholder: return "Placeholder";
    case CXCompletionChunk_Informative: return "Informative";
    case CXCompletionChunk_CurrentParameter: return "CurrentParameter";
    case CXCompletionChunk_LeftParen: return "LeftParen";
    case CXCompletionChunk_RightParen: return "RightParen";
    case CXCompletionChunk_LeftBracket: return "LeftBracket";
    case CXCompletionChunk_RightBracket: return "RightBracket";
    case CXCompletionChunk_LeftBrace: return "LeftBrace";
    case CXCompletionChunk_RightBrace: return "RightBrace";
    case CXCompletionChunk_LeftAngle: return "LeftAngle";
    case CXCompletionChunk_RightAngle: return "RightAngle";
    case CXCompletionChunk_Comma: return "Comma";
    case CXCompletionChunk_ResultType: return "ResultType";
    case CXCompletionChunk_Colon: return "Colon";
    case CXCompletionChunk_SemiColon: return "SemiColon";
    case CXCompletionChunk_Equal: return "Equal";
    case CXCompletionChunk_HorizontalSpace: return "HorizontalSpace";
    case CXCompletionChunk_VerticalSpace: return "VerticalSpace";
    }
    return "";
}


bool locationFromString(const QByteArray &string, Path *path, int *line, int *column)
{
    QRegExp locationRegExp = QRegExp("(.*):([0-9]+):([0-9]+)");
    if (!locationRegExp.exactMatch(QString::fromLocal8Bit(string)))
        return false;

    if (path)
        *path = Path::resolved(locationRegExp.cap(1).toLocal8Bit());
    if (line)
        *line = locationRegExp.cap(2).toInt();
    if (column)
        *column = locationRegExp.cap(3).toInt();
    return true;
}
