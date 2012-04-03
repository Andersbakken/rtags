#include "Database.h"
#include "DumpJob.h"
#include "FollowLocationJob.h"
#include "Indexer.h"
#include "MatchJob.h"
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

struct DatabaseImpl
{
    Database* db;
    int jobId;
};


static inline bool isCursorReference(CXCursorKind kind)
{
    return (kind >= CXCursor_FirstRef && kind <= CXCursor_LastRef);
}

Database::Database(QObject* parent)
    : QObject(parent), mImpl(new DatabaseImpl)
{
    mImpl->db = this;
    mImpl->jobId = 0;

    qRegisterMetaType<QList<QByteArray> >("QList<QByteArray>");
}

Database::~Database()
{
    delete mImpl;
}

int Database::nextId()
{
    ++mImpl->jobId;
    if (!mImpl->jobId)
        ++mImpl->jobId;
    return mImpl->jobId;
}


int Database::poke(const QueryMessage &query)
{
    const int id = nextId();
    // const bool exists = Resource(query.query().front()).exists(Resource::Information);
    // QMetaObject::invokeMethod(this, "complete", Qt::QueuedConnection,
    //                           Q_ARG(int, id),
    //                           Q_ARG(QList<QByteArray>, QList<QByteArray>() << (exists ? "success" : "failure")));
    // PokeJob* job = new PokeJob(query.query().first(), id);
    // QThreadPool::globalInstance()->start(job);
    return id;

}

int Database::followLocation(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc)) {
        error("Failed to make location from [%s]", query.query().front().constData());
        return 0;
    }

    const int id = nextId();

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

    log(1) << "references for location" << loc.path << loc.offset;

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
    log(1) << "references for name" << name;

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

    log(1) << "recompile" << fileName;

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

    log(1) << "match" << partial;

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

    log(1) << "dump" << partial;

    DumpJob* job = new DumpJob(partial, id);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::status(const QueryMessage &)
{
    const int id = nextId();

    log(1) << "status";

    StatusJob* job = new StatusJob(id);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);
    return id;
}


static const char* const dbNames[] = { "dependencies.db", "symbols.db", "symbolnames.db", "fileinfos.db" };

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

