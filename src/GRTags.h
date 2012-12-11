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
        Dump,
        FindSymbols,
        ListSymbols,
        FindReferences,
        FindAll
    };
    void findSymbols(const ByteArray &pattern);
    void listSymbols(const ByteArray &pattern);
    bool load(const Path &db);
    bool save();
    void dump();
    void dirty();
    int parseFiles();
    static Path::VisitResult visit(const Path &path, void *userData);
    List<ByteArray> mFilters;
    Map<uint32_t, time_t> mFiles;
    // file id to last modified, time_t means currently parsing
    Map<ByteArray, Map<Location, bool> > mSymbols;
    // symbolName to Map<location, bool> bool == false means cursor, true means reference

    leveldb::DB *mDB;
    Path mPath;
    Mode mMode;
    unsigned mKeyFlags;
    List<Path> mPending;
    Set<uint32_t> mDirty;
};

#endif
