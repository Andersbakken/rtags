#ifndef LevelDB_h
#define LevelDB_h

#include <leveldb/db.h>
#include <errno.h>
#include "Server.h"

class LevelDB
{
public:
    LevelDB();
    ~LevelDB();
    enum Mode {
        ReadOnly,
        ReadWrite
    };
    bool open(Server::DatabaseType type, Mode mode, QByteArray *error = 0);
    void close();
    leveldb::DB *db() const { return mDB; }
private:
    leveldb::DB *mDB;
    int mType;
};

#endif
