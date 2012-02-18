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
        : errorFd(0), flags(0), db(0), pendingJobs(0), index(0)
    {
        extern QHash<Path, unsigned> *filesByNameDebugUgleHack;
        filesByNameDebugUgleHack = &filesByName;
    }

    int errorFd;
    Path errorFn;
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
    QSet<QByteArray> pchFromUnsaved; // same key as pch, if present in the set
                                     // the pch header or one of the headers it
                                     // included (directly or indirectly) were
                                     // supplied by CXUnsavedFile
    QVector<CXUnsavedFile> unsavedFiles;
    QHash<Path, QByteArray> unsavedFilesHash;
    QThreadPool threadPool;

    QList<Source> sources;
    QMutex mutex;

    bool initErrorFD()
    {
        if (errorFd <= 0) {
            char tmp[256];
            strncpy(tmp, "/tmp/rtags.errors.XXXXXX", 255);
            errorFd = mkstemp(tmp);
            if (errorFd > 0)
                errorFn = tmp;
        }
        return errorFd > 0;
    }

    inline int locationKey(const Location &loc, char buf[512]) const
    {
        if (!loc.file)
            return 0;
        const int ret = snprintf(buf, 512, "%d:%d:%d", loc.file, loc.line, loc.column);
        return ret;
    }
};

#endif
