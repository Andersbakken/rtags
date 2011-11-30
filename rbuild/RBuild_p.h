#ifndef RBuild_p_h
#define RBuild_p_h

#include "CursorKey.h"
#include "AtomicString.h"
#include "GccArguments.h"
#include "RBuild.h"
#include <QList>
#include <QHash>

struct Location
{
    static QMap<AtomicString, unsigned> *sFiles;
    Location()
        : file(0), line(0), column(0)
    {}

    unsigned file, line, column;
    inline AtomicString fileName() const
    {
        if (file) {
            for (QMap<AtomicString, unsigned>::const_iterator it = sFiles->begin(); it != sFiles->end(); ++it) {
                if (it.value() == file)
                    return it.key();
            }
        }
        return AtomicString();
    }
    inline QByteArray key() const
    {
        if (!file)
            return QByteArray();
        char buf[1024];
        const int ret = snprintf(buf, 1024, "%s:%d:%d", fileName().constData(), line, column);
        return QByteArray(buf, ret);
    }
    inline bool operator==(const Location &other) const
    {
        return file == other.file && line == other.line && column == other.column;
    }
};

static inline QDataStream &operator<<(QDataStream &ds, const Location &loc)
{
    ds << loc.file << loc.line << loc.column;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, Location &loc)
{
    ds >> loc.file >> loc.line >> loc.column;
    return ds;
}


static inline QDebug operator<<(QDebug dbg, const Location &location)
{
    QString str = "Location(";
    if (!location.file) {
        str += ")";
    } else {
        str += location.key() + ")";
    }
    dbg << str;
    return dbg;
}

struct Entity {
    Entity() : kind(CXIdxEntity_Unexposed) {}
    AtomicString name;
    QList<AtomicString> parentNames;
    CXIdxEntityKind kind;
    Location definition;
    QSet<Location> declarations;
    QHash<Location, AtomicString> references; // value is containingFunction
};

struct Data {
    QByteArray name, target;
    QSet<QByteArray> references;
};

static inline QDataStream &operator<<(QDataStream &ds, const Data &data)
{
    ds << data.name << data.target << data.references;
    return ds;
}
static inline QDataStream &operator>>(QDataStream &ds, Data &data)
{
    ds >> data.name >> data.target >> data.references;
    return ds;
}

static inline uint qHash(const Location &l)
{
    // ### is this good?
    return (l.file << 1) + (l.line << 2) + (l.column << 3);
}

struct RBuildPrivate
{
    RBuildPrivate()
        : db(0), mPendingJobs(0), mIndex(0)
    {
        Location::sFiles = &files;
    }

    QHash<AtomicString, Entity> entities;
    QMap<AtomicString, unsigned> files;
    QHash<AtomicString, QList<Location> > references;
    leveldb::DB *db;
    Path mMakefile, mSourceDir, mDBPath;
    MakefileParser mParser;
    int mPendingJobs;
    CXIndex mIndex;
    QHash<Precompile*, QList<GccArguments> > mFilesByPrecompile;
    QList<GccArguments> mFiles;
    QThreadPool mThreadPool;

    struct Dependencies {
        Path file;
        QList<QByteArray> arguments;
        quint64 lastModified;
        QHash<Path, quint64> dependencies;
    };
    QList<Dependencies> dependencies;
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
