#ifndef Project_h
#define Project_h

#include <tr1/memory>
#include <Path.h>
#include <ReadWriteLock.h>

class Database;
class Indexer;
class GRTags;
class ScopedDB;
class Project
{
public:
    enum DatabaseType {
        Symbol,
        SymbolName,
        Dependency,
        FileInformation,
        GRFiles,
        GR,
        DatabaseTypeCount
    };

    Project();
    ~Project();

    Database *databases[DatabaseTypeCount];
    Indexer *indexer;
    GRTags *grtags;

    Path srcRoot, projectPath;

    Path databaseDir(DatabaseType type) const;

    ScopedDB db(DatabaseType type, ReadWriteLock::LockType lockType) const;
};

#endif
