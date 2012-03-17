#include "Database.h"
#include "Indexer.h"
#include "Path.h"
#include "QueryMessage.h"
#include "Resource.h"
#include "Rdm.h"
#include "UnitCache.h"
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
#include "CodeCompleteJob.h"
#include "CursorInfoJob.h"
#include "DumpJob.h"
#include "FollowLocationJob.h"
#include "MatchJob.h"
#include "RecompileJob.h"
#include "ReferencesJob.h"

QByteArray Database::s_base;

Q_DECLARE_METATYPE(QList<QByteArray>)

struct DatabaseImpl
{
    Database* db;
    int lastJobId;
};


static inline bool isCursorReference(CXCursorKind kind)
{
    return (kind >= CXCursor_FirstRef && kind <= CXCursor_LastRef);
}

Database::Database(QObject* parent)
    : QObject(parent), m_impl(new DatabaseImpl)
{
    m_impl->db = this;
    m_impl->lastJobId = 0;

    qRegisterMetaType<QList<QByteArray> >("QList<QByteArray>");
}

Database::~Database()
{
    delete m_impl;
}

int Database::followLocation(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc))
        return -1;

    const int id = ++m_impl->lastJobId;

    FollowLocationJob* job = new FollowLocationJob(id, loc);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::cursorInfo(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc))
        return -1;
    const int id = ++m_impl->lastJobId;
    CursorInfoJob* job = new CursorInfoJob(id, loc);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);
    return id;
}

int Database::codeComplete(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc))
        return -1;

    const int id = ++m_impl->lastJobId;

    CodeCompleteJob* job = new CodeCompleteJob(id, loc, query.unsavedFiles());
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);
    return id;
}

int Database::referencesForLocation(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc))
        return -1;

    const int id = ++m_impl->lastJobId;

    log(1) << "references for location" << loc.path << Resource::hash(loc.path) << loc.line << loc.column
           << loc.offset;

    ReferencesJob* job = new ReferencesJob(id, loc);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::referencesForName(const QueryMessage& query)
{
    const int id = ++m_impl->lastJobId;

    const QByteArray name = query.query().front();
    log(1) << "references for name" << name;

    ReferencesJob* job = new ReferencesJob(id, name);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}
int Database::recompile(const QueryMessage &query)
{
    const QByteArray fileName = query.query().front();
    const int id = ++m_impl->lastJobId;

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
    const int id = ++m_impl->lastJobId;

    log(1) << "match" << partial;

    MatchJob* job = new MatchJob(partial, id);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Database::dump(const QueryMessage &query)
{
    const QByteArray partial = query.query().front();
    const int id = ++m_impl->lastJobId;

    log(1) << "dump" << partial;

    DumpJob* job = new DumpJob(partial, id);
    connect(job, SIGNAL(complete(int, QList<QByteArray>)),
            this, SIGNAL(complete(int, QList<QByteArray>)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

static const char* const dbNames[] = { "/includes.db", "/defines.db",
                                       "/references.db", "/symbols.db" };

QByteArray Database::databaseName(Type type)
{
    if (s_base.isEmpty())
        return QByteArray();
    return s_base + dbNames[type];
}

void Database::setBaseDirectory(const QByteArray& base)
{
    s_base = base;
}
