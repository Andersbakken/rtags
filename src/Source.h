#ifndef Source_h
#define Source_h

#include "Path.h"

struct Source {
    GccArguments args;
    uint64_t lastModified;
    Map<Path, uint64_t> dependencies;
    bool fromUnsavedFile;
};

#endif
