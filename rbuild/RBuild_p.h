#ifndef RBuild_p_h
#define RBuild_p_h

#include "AtomicString.h"
#include "GccArguments.h"
#include "RBuild.h"
#include <QList>
#include <QHash>
#include <RTags.h>
#include <Source.h>
#include <Location.h>
#include <Database.h>

struct Entity {
    Entity() : kind(CXIdxEntity_Unexposed) {}
    QByteArray name;
    QList<QByteArray> parentNames;
    CXIdxEntityKind kind;
    Location definition;
    QSet<Location> declarations, references, extraDeclarations;
};

struct PendingReference
{
    QByteArray usr, specialized;
    Location location;
};

struct RBuildPrivate
{
    RBuildPrivate()
        : flags(0), db(0), pendingJobs(0), index(0)
    {
        Location::files() = &filesByName;
    }

    unsigned flags;
    QHash<QByteArray, Entity> entities;
    QList<PendingReference> pendingReferences;
    QHash<Path, unsigned> filesByName;
    Database *db;
    Path sourceDir, dbPath;
    QSet<MakefileParser*> makefileParsers;
    int pendingJobs;
    CXIndex index;
    QList<GccArguments> files;
    QList<QByteArray> extraArgs;
    QThreadPool threadPool;

    QList<Source> sources;
    QMutex entryMutex;

    inline int locationKey(const Location &loc, char buf[512]) const
    {
        if (!loc.file)
            return 0;
        const int ret = snprintf(buf, 512, "%d:%d:%d", loc.file, loc.line, loc.column);
        return ret;
    }
};

#endif
