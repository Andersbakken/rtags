#include "SourceRange.h"
#include "AST.h"

SourceLocation SourceRange::start() const
{
    return AST::createLocation(clang_getRangeStart(range));
}

SourceLocation SourceRange::end() const
{
    return AST::createLocation(clang_getRangeEnd(range));
}
