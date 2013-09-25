/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef GRTags_h
#define GRTags_h

#include <rct/String.h>
#include "Location.h"
#include <rct/Hash.h>
#include <rct/Path.h>
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
    void findSymbols(const String &pattern);
    void listSymbols(const String &pattern);
    void paths(const String &pattern);
    bool load(const Path &db);
    bool save();
    void dump();
    int parseFiles();
    static Path::VisitResult visit(const Path &path, void *userData);
    List<String> mFilters;
    Hash<uint32_t, time_t> mFiles;
    // file id to last modified, time_t means currently parsing
    Hash<String, Hash<Location, bool> > mSymbols;
    // symbolName to Hash<location, bool> bool == false means cursor, true means reference

    leveldb::DB *mDB;
    Path mPath;
    Mode mMode;
    unsigned mFlags;
    unsigned mKeyFlags;
    List<Path> mPending;
    Set<uint32_t> mDirty;
};

#endif
