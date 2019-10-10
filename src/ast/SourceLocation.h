#ifndef SOURCELOCATION_H
#define SOURCELOCATION_H

#include <Location.h>
#include <string>
#include <rct/String.h>

struct SourceLocation {
    SourceLocation()
    {}

    bool operator<(const SourceLocation &other) const { return mLocation < other.mLocation; }

    bool isNull() const { return mLocation.isNull(); }
    int offset() { return mOffset; }

    int line() const { return mLocation.line(); }
    int column() const { return mLocation.column(); }
    std::string file() const { return mLocation.path(); }
    std::string toString() const
    {
        return String::format<256>("%s,%d",
                                   mLocation.toString(Location::AbsolutePath|Location::NoColor).constData(),
                                   mOffset);
    }


    Location mLocation;
    int mOffset { -1 };
};

#endif /* SOURCELOCATION_H */
