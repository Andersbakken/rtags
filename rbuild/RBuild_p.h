#ifndef RBuild_p_h
#define RBuild_p_h

#include "GccArguments.h"
#include "RBuild.h"
#include <QList>
#include <QHash>
#include <RTags.h>
#include <Source.h>
#include <Location.h>
#include <Database.h>

struct PendingReference {
    QByteArray usr, specialized;
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

    QHash<Location, PendingReference> pendingReferences;
    QHash<Path, unsigned> filesByName;
    Database *db;
    Path sourceDir, dbPath;
    QSet<MakefileParser*> makefileParsers;
    int pendingJobs;
    CXIndex index;
    QList<GccArguments> files; // used for detecting duplicate files only
    QList<QByteArray> systemIncludes;
    QList<QByteArray> extraArgs; // -I and -D passed on command line
    QHash<QByteArray, QPair<Path, Path> > pch; // QPair(pch, header)
    QSet<QByteArray> pchFromUnsaved;
    QVector<CXUnsavedFile> unsavedFiles;
    QHash<Path, QByteArray> unsavedFilesHash;
    QThreadPool threadPool;

    QList<Source> sources;
    QMutex mutex;

    inline int locationKey(const Location &loc, char buf[512]) const
    {
        if (!loc.file)
            return 0;
        const int ret = snprintf(buf, 512, "%d:%d:%d", loc.file, loc.line, loc.column);
        return ret;
    }
};

#endif
