#ifndef SOURCELOCATION_H
#define SOURCELOCATION_H

#include <Location.h>
#include <string>
#include <rct/String.h>
#include <RTags.h>

class SourceLocation {
public:
    SourceLocation(const CXSourceLocation &location)
        : mCXLocation(location)
    {}

    bool operator<(const SourceLocation &other) const { materialize(); other.materialize(); return mLocation < other.mLocation; }

    bool isNull() const { materialize(); return mLocation.isNull(); }
    int offset() { materialize(); return mOffset; }

    int line() const { materialize(); return mLocation.line(); }
    int column() const { materialize(); return mLocation.column(); }
    std::string file() const { materialize(); return mLocation.path(); }
    std::string toString() const
    {
        materialize();
        return mLocation.toString(Location::AbsolutePath|Location::NoColor);
    }
private:
    void materialize() const
    {
        if (mOffset == -2) {
            mLocation = RTags::createLocation(mCXLocation, &mOffset);
        }
    }
    const CXSourceLocation mCXLocation;
    mutable Location mLocation;
    mutable int mOffset { -2 };
};

#endif /* SOURCELOCATION_H */
