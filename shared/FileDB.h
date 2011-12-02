#ifndef FILEDB_H
#define FILEDB_H

#include "Database.h"
#include "Path.h"

class FileDB : public Database
{
public:
    FileDB();
    ~FileDB();

    virtual bool isOpened() const;
    virtual iterator *createIterator(ConnectionType) const;

protected:
    virtual bool openDatabase(const Path &db, Mode mode);
    virtual void closeDatabase();
    virtual Connection *createConnection(ConnectionType type);

private:
    Path mPath;
    Mode mMode;
};

#endif // FILEDB_H
