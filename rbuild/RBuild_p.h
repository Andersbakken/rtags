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
    Path makefile, sourceDir, dbPath;
    MakefileParser parser;
    int pendingJobs;
    CXIndex index;
    QHash<Precompile*, QList<GccArguments> > filesByPrecompile;
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

class Precompile;
class PrecompileRunnable : public QObject, public QRunnable
{
    Q_OBJECT
public:
    PrecompileRunnable(Precompile *pch,
                       RBuildPrivate *rbp,
                       CXIndex index) // ### is this threadsafe?
        : mPch(pch), mRBP(rbp), mIndex(index)
    {
        setAutoDelete(true);
    }
    virtual void run();
signals:
    void finished(Precompile *pre);
private:
    Precompile *mPch;
    RBuildPrivate *mRBP;
    CXIndex mIndex;
};

#endif
