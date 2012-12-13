#ifndef GRTags_h
#define GRTags_h

#include "ByteArray.h"
#include "Location.h"
#include "Map.h"
#include "Path.h"
#include <leveldb/db.h>
#include <leveldb/comparator.h>

class GRTags
{
public:
    GRTags();
    bool exec(int argc, char **argv);
private:
    enum Mode {
        Create,
        Detect,
        Dump,
        FindAll,
        FindReferences,
        FindSymbols,
        ListSymbols,
        Paths,
        Update
    };
    enum Flag {
        None = 0x0,
        MatchCaseInsensitive = 0x1,
        PreferExact = 0x2,
        AbsolutePath = 0x4
    };
    void findSymbols(const ByteArray &pattern);
    void listSymbols(const ByteArray &pattern);
    void paths(const ByteArray &pattern);
    bool load(const Path &db);
    bool save();
    void dump();
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
    unsigned mFlags;
    unsigned mKeyFlags;
    List<Path> mPending;
    Set<uint32_t> mDirty;
};

#endif
