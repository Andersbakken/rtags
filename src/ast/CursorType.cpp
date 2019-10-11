#include "CursorType.h"
#include "AST.h"

std::string CursorType::spelling() const
{
    return AST::toString(clang_getTypeSpelling(type));
}
std::shared_ptr<Cursor> CursorType::declaration() const
{
    return ast->create(clang_getTypeDeclaration(type));
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
