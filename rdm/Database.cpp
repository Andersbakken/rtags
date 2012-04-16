#include "Database.h"
#include "DumpJob.h"
#include "FollowLocationJob.h"
#include "Indexer.h"
#include "MatchJob.h"
#include "PollJob.h"
#include "Path.h"
#include "QueryMessage.h"
#include "Rdm.h"
#include "RecompileJob.h"
#include "ReferencesJob.h"
#include "StatusJob.h"
#include "leveldb/db.h"
#include <QDateTime>
#include <QDebug>
#include <QFile>
#include <QHash>
#include <QMetaType>
#include <QMutex>
#include <QMutexLocker>
#include <QRunnable>
#include <QThreadPool>
#include <QWaitCondition>
#include <clang-c/Index.h>

QByteArray Database::sBase;

Q_DECLARE_METATYPE(QList<QByteArray>)
static inline bool isCursorReference(CXCursorKind kind)
{
    return (kind >= CXCursor_FirstRef && kind <= CXCursor_LastRef);
}

Database::Database(QObject *parent, Indexer *indexer)
    : QObject(parent), mJobId(0), mIndexer(indexer)
{
    qRegisterMetaType<QList<QByteArray> >("QList<QByteArray>");
}

int Database::nextId()
{
    ++mJobId;
    if (!mJobId)
        ++mJobId;
    return mJobId;
}

int Database::followLocation(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc)) {
        error("Failed to make location from [%s]", query.query().front().constData());
        return 0;
    }

    const int id = nextId();

    warning() << "followLocation" << loc;

    FollowLocationJob* job = new FollowLocationJob(id, loc, !(query.flags() & QueryMessage::NoContext));
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::referencesForLocation(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc))
        return 0;

    const int id = nextId();

    warning() << "references for location" << loc;

    ReferencesJob* job = new ReferencesJob(id, loc, !(query.flags() & QueryMessage::NoContext));
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::referencesForName(const QueryMessage& query)
{
    const int id = nextId();

    const QByteArray name = query.query().front();
    warning() << "references for name" << name;

    ReferencesJob* job = new ReferencesJob(id, name, !(query.flags() & QueryMessage::NoContext));
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}
int Database::recompile(const QueryMessage &query)
{
    const QByteArray fileName = query.query().front();
    const int id = nextId();

    warning() << "recompile" << fileName;

    // ### this needs to be implemented

    RecompileJob* job = new RecompileJob(fileName, id);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::match(const QueryMessage &query)
{
    const QByteArray partial = query.query().front();
    const int id = nextId();

    warning() << "match" << partial;

    MatchJob* job = new MatchJob(partial, id, query.type(), !(query.flags() & QueryMessage::NoContext));
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::dump(const QueryMessage &query)
{
    const QByteArray partial = query.query().front();
    const int id = nextId();

    warning() << "dump" << partial;

    DumpJob* job = new DumpJob(partial, id);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::status(const QueryMessage &)
{
    const int id = nextId();

    warning() << "status";

    // ### this needs to be implemented


    StatusJob* job = new StatusJob(id);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);
    return id;
}

int Database::poll(const QueryMessage &)
{
    const int id = nextId();

    warning() << "poll";
    // ### this needs to be implemented

    PollJob *job = new PollJob(mIndexer, id);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);
    return id;
}


static const char* const dbNames[] = {
    "general.db",
    "dependencies.db",
    "symbols.db",
    "symbolnames.db",
    "fileinfos.db"
};

QByteArray Database::databaseName(Type type)
{
    if (sBase.isEmpty())
        return QByteArray();
    return sBase + dbNames[type];
}

void Database::setBaseDirectory(const QByteArray& base)
{
    sBase = base;
    Q_ASSERT(sBase.endsWith('/'));
}

