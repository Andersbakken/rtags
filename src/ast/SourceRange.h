#ifndef SOURCERANGE_H
#define SOURCERANGE_H

#include <clang-c/Index.h>
#include "SourceLocation.h"
#include <string>

struct SourceRange {
    SourceRange(const CXSourceRange &r = clang_getNullRange())
        : range(r)
    {}
    CXSourceRange range;

    SourceLocation start() const;
    SourceLocation end() const;
    int length() const { return end().offset() - start().offset(); }
    std::string toString() const { return start().toString() + " - " + end().toString(); }
};



#endif /* SOURCERANGE_H */
