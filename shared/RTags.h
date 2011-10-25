#ifndef RTags_h
#define RTags_h

#include <string>
#include <stdlib.h>
#include <QtCore>
#include <leveldb/db.h>
#include <Path.h>

namespace RTags {
QDataStream &operator<<(QDataStream &ds, time_t t);
QDataStream &operator>>(QDataStream &ds, time_t &t);
bool parseLocation(const std::string &string,
                   std::string &file, unsigned &line, unsigned &col);
Path findRtagsDb();
class LevelDBScope
{
public:
    LevelDBScope(leveldb::DB *d)
        : db(d)
    {}
    ~LevelDBScope()
    {
        delete db;
    }
private:
    leveldb::DB *db;
};
};

#endif
