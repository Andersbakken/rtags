#ifndef GRTags_h
#define GRTags_h

#include "ByteArray.h"
#include "Location.h"
#include "Map.h"
#include "Path.h"
#include <leveldb/comparator.h>
#include <leveldb/db.h>

class LocationComparator;
class GRTags
{
public:
    GRTags();
    bool exec(int argc, char **argv);
private:
    enum Mode {
        Detect,
        Update,
        Create,
        Dump
    };
    void parse(const Path &sourceFile);
    bool load(const Path &db, Mode mode);
    bool save();
    void dump();
    static Path::VisitResult visit(const Path &path, void *userData);
    List<ByteArray> mFilters;
    Map<uint32_t, time_t> mFiles;
    // file id to last modified, time_t means currently parsing
    Map<ByteArray, Map<Location, bool> > mSymbols;
    // symbolName to Map<location, bool> bool == false means cursor, true means reference

    leveldb::DB *mDB;
    Path mPath;
};

#endif
