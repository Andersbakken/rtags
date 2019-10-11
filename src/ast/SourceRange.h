#ifndef SOURCERANGE_H
#define SOURCERANGE_H

#include <clang-c/Index.h>
#include "SourceLocation.h"
#include <string>

class SourceRange
{
public:
    SourceRange(const CXSourceRange &r = clang_getNullRange());

    std::shared_ptr<SourceLocation> start() const { return mStart; }
    std::shared_ptr<SourceLocation> end() const { return mEnd; }
    int length() const { return mEnd->offset() - mStart->offset(); }
    std::string toString() const { return mStart->toString() + " - " + mEnd->toString(); }
private:
    std::shared_ptr<SourceLocation> mStart, mEnd;
};


#endif /* SOURCERANGE_H */
