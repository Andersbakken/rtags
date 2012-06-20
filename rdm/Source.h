#ifndef Source_h
#define Source_h

#include <QtCore>
#include "Path.h"

struct Source {
    GccArguments args;
    uint64_t lastModified;
    Map<Path, uint64_t> dependencies;
    bool fromUnsavedFile;
};

static inline QDebug operator<<(QDebug dbg, const Source &s)
{
    dbg << s.args << s.lastModified << s.dependencies << s.fromUnsavedFile;
    return dbg;
}

#endif
