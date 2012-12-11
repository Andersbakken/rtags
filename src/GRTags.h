#ifndef GRTags_h
#define GRTags_h

#include "Location.h"

class GRTags
{
public:
    GRTags();
    bool exec(int argc, char **argv);
private:
    void parse(const Path &sourceFile);
    void load(const Path &db);
    void save(const Path &db);
    static Path::VisitResult visit(const Path &path, void *userData);
    List<ByteArray> mFilters;
    Map<uint32_t, time_t> mFiles;
    // file id to last modified, time_t means currently parsing
    Map<ByteArray, Map<Location, bool> > mSymbols;
    // symbolName to Map<location, bool> bool == false means cursor, true means reference
};

#endif
