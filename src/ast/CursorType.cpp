#include "CursorType.h"
#include "AST.h"

std::string CursorType::spelling() const
{
    return AST::toString(clang_getTypeSpelling(type));
}
Cursor CursorType::declaration() const
{
    return ast ? ast->create(clang_getTypeDeclaration(type)) : Cursor();
}
std::string CursorType::callingConvention() const
{
    return AST::toString(clang_getFunctionTypeCallingConv(type));
}
std::string CursorType::referenceType() const
{
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
    return AST::toString(clang_Type_getCXXRefQualifier(type));
#else
    return std::string();
#endif
}
