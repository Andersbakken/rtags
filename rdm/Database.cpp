#include "Database.h"
#include "Indexer.h"
#include "Path.h"
#include "QueryMessage.h"
#include "Resource.h"
#include "Tools.h"
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

class DatabaseImpl : public QObject
{
    Q_OBJECT
public:
    int followLocation(const QByteArray& fileName, int line, int col);
    int cursorInfo(const QByteArray& fileName, int line, int col);
    int codeComplete(const QByteArray& fileName, int line, int col, const QHash<Path, QByteArray> &unsaved);
    int referencesForLocation(const QByteArray& fileName, int line, int col);
    int referencesForName(const QByteArray& name);
    int recompile(const QByteArray& fileName);
    int match(const QByteArray& partial);
    int dump(const QByteArray& fileName);

    Database* db;
    int lastJobId;

signals:
    void complete(int id, const QList<QByteArray>& locations);
};

#include "Database.moc"

int DatabaseImpl::referencesForLocation(const QByteArray& fileName, int line, int col)
{
    const int id = ++lastJobId;

    qDebug() << "references for location" << fileName << Resource::hash(fileName) << line << col;

    ReferencesJob* job = new ReferencesJob(fileName, id, line, col);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int DatabaseImpl::referencesForName(const QByteArray& name)
{
    const int id = ++lastJobId;

    qDebug() << "references for name" << name;

    ReferencesJob* job = new ReferencesJob(name, id, -1, -1);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int DatabaseImpl::recompile(const QByteArray& fileName)
{
    const int id = ++lastJobId;

    qDebug() << "recompile" << fileName;

    RecompileJob* job = new RecompileJob(fileName, id);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int DatabaseImpl::match(const QByteArray& partial)
{
    const int id = ++lastJobId;

    qDebug() << "match" << partial;

    MatchJob* job = new MatchJob(partial, id);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int DatabaseImpl::dump(const QByteArray& partial)
{
    const int id = ++lastJobId;

    qDebug() << "dump" << partial;

    DumpJob* job = new DumpJob(partial, id);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}


static inline bool isCursorReference(CXCursorKind kind)
{
    return (kind >= CXCursor_FirstRef && kind <= CXCursor_LastRef);
}

int DatabaseImpl::followLocation(const QByteArray& fileName, int line, int col)
{
    const int id = ++lastJobId;

    FollowLocationJob* job = new FollowLocationJob(fileName, id, line, col);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

int DatabaseImpl::cursorInfo(const QByteArray& fileName, int line, int col)
{
    const int id = ++lastJobId;

    CursorInfoJob* job = new CursorInfoJob(fileName, id, line, col);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}


int DatabaseImpl::codeComplete(const QByteArray& fileName, int line, int col,
                               const QHash<Path, QByteArray> &unsaved)
{
    const int id = ++lastJobId;

    CodeCompleteJob* job = new CodeCompleteJob(fileName, id, line, col, unsaved);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

Database::Database(QObject* parent)
    : QObject(parent), m_impl(new DatabaseImpl)
{
    m_impl->db = this;
    m_impl->lastJobId = 0;

    qRegisterMetaType<QList<QByteArray> >("QList<QByteArray>");
    connect(m_impl, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
}

Database::~Database()
{
    delete m_impl;
}

int Database::followLocation(const QueryMessage &query)
{
    Location loc;
    if (!makeLocation(query.query().front(), &loc))
        return -1;

    return m_impl->followLocation(loc.path, loc.line, loc.column);
}

int Database::cursorInfo(const QueryMessage &query)
{
    Location loc;
    if (!makeLocation(query.query().front(), &loc))
        return -1;

    return m_impl->cursorInfo(loc.path, loc.line, loc.column);
}

int Database::codeComplete(const QueryMessage &query)
{
    Location loc;
    if (!makeLocation(query.query().front(), &loc))
        return -1;

    return m_impl->codeComplete(loc.path, loc.line, loc.column, query.unsavedFiles());
}

int Database::referencesForLocation(const QueryMessage &query)
{
    Location loc;
    if (!makeLocation(query.query().front(), &loc))
        return -1;

    return m_impl->referencesForLocation(loc.path, loc.line, loc.column);
}

int Database::referencesForName(const QueryMessage& query)
{
    return m_impl->referencesForName(query.query().front());
}

int Database::recompile(const QueryMessage &query)
{
    return m_impl->recompile(query.query().front());
}

int Database::match(const QueryMessage &query)
{
    return m_impl->match(query.query().front());
}

int Database::dump(const QueryMessage &query)
{
    return m_impl->dump(query.query().front());
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
