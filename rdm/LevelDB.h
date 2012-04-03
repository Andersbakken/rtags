#ifndef LevelDB_h
#define LevelDB_h

#include <leveldb/db.h>
#include <errno.h>
#include "Database.h"

class LevelDB
{
public:
    LevelDB();
    ~LevelDB();
    enum Mode {
        ReadOnly,
        ReadWrite
    };
    bool open(Database::Type type, Mode mode);
    leveldb::DB *db() const { return mDB; }
private:
    leveldb::DB *mDB;
    int mType;
};

#endif
