#include "SourceRange.h"
#include "AST.h"

SourceRange::SourceRange(const CXSourceRange &range)
    : mStart(AST::createLocation(clang_getRangeStart(range))), mEnd(AST::createLocation(clang_getRangeEnd(range)))
{
}

