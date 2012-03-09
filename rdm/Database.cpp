#include "Database.h"
#include "Indexer.h"
#include "Resource.h"
#include "UnitCache.h"
#include "Path.h"
#include "leveldb/db.h"
#include <clang-c/Index.h>
#include <QWaitCondition>
#include <QMutex>
#include <QMutexLocker>
#include <QRunnable>
#include <QThreadPool>
#include <QMetaType>
#include <QDateTime>
#include <QHash>
#include <QFile>
#include <QDebug>

QByteArray Database::s_base;

Q_DECLARE_METATYPE(QList<QByteArray>)

class DatabaseImpl : public QObject
{
    Q_OBJECT
public:
    int followLocation(const QByteArray& filename, int row, int col);
    int referencesForLocation(const QByteArray& filename, int row, int col);
    int referencesForName(const QByteArray& name);
    int recompile(const QByteArray& filename);
    int match(const QByteArray& partial);

    Database* db;
    int lastJobId;

signals:
    void complete(int id, const QList<QByteArray>& locations);
};

class FollowLocationJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    FollowLocationJob(const QByteArray& fn, int i, int l, int c);
    ~FollowLocationJob();

signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();

private:
    QByteArray filename;
    int id, line, col;
};

class ReferencesJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    ReferencesJob(const QByteArray& fn, int i, int l, int c);
    ~ReferencesJob();

signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();
    void runLocation();
    void runName();

private:
    QByteArray filename;
    int id, line, col;
};

class RecompileJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    RecompileJob(const QByteArray& fn, int i);
    ~RecompileJob();

signals:
    void complete(int id, const QList<QByteArray>&);

protected:
    void run();

private:
    QByteArray filename;
    int id;
};

class MatchJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    MatchJob(const QByteArray& p, int i);
    ~MatchJob();

signals:
    void complete(int id, const QList<QByteArray>&);

protected:
    void run();

private:
    QByteArray partial;
    int id;
};

#include "Database.moc"

typedef bool (*VisitFile)(UnitCache::Unit* unit, void* data);

static void visitIncluderFiles(const QByteArray& filename, VisitFile visitor, void* data)
{
    qDebug() << "looking at" << filename;

    QByteArray dbname = Database::databaseName(Database::Include);
    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), dbname.constData(), &db);
    if (!status.ok())
        return;
    std::string value;
    db->Get(leveldb::ReadOptions(), filename.constData(), &value);
    delete db;
    if (value.empty())
        return;

    QByteArray bvalue = QByteArray::fromRawData(value.c_str(), value.size());
    QList<QByteArray> others = bvalue.split('\n');
    foreach(const QByteArray& inc, others) {
        if (inc.isEmpty())
            continue;
        qDebug() << "about to visit" << inc;

        CachedUnit unit(inc, UnitCache::AST);
        if (unit.unit()) {
            if ((*visitor)(unit.unit(), data))
                break;
        } else
            qWarning("Unit not found: %s", inc.constData());
    }
}

struct DefinitionData
{
    QByteArray filename;
    unsigned int line, col;
    QByteArray result;
};

static bool visitFindDefinition(UnitCache::Unit* ud, void* data)
{
    CXTranslationUnit unit = ud->unit;
    DefinitionData* defdata = static_cast<DefinitionData*>(data);
    CXFile file = clang_getFile(unit, defdata->filename.constData());
    CXSourceLocation loc = clang_getLocation(unit, file, defdata->line, defdata->col);
    CXCursor cursor = clang_getCursor(unit, loc);
    if (clang_equalCursors(cursor, clang_getNullCursor())) {
        qDebug() << "null at the get-go";
        return false;
    }
    CXCursor def = clang_getCursorDefinition(cursor);
    if (!clang_equalCursors(def, clang_getNullCursor())) {
        loc = clang_getCursorLocation(def);

        CXFile rfile;
        unsigned int rrow, rcol, roff;
        clang_getSpellingLocation(loc, &rfile, &rrow, &rcol, &roff);
        CXString filename = clang_getFileName(rfile);

        qDebug() << "found! definition" << clang_getCString(filename) << rrow << rcol << roff;
        defdata->result = QByteArray(Path::resolved(clang_getCString(filename)))
            + ":" + QByteArray::number(rrow)
            + ":" + QByteArray::number(rcol)
            + ":" + QByteArray::number(roff);

        clang_disposeString(filename);
        return true;
    } else {
        qDebug() << "bah!";
    }
    return false;
}

struct ReferenceData
{
    const char* usr;
    int usrlen;
    QSet<QByteArray> results;
};

static CXChildVisitResult visitUnitFindReferences(CXCursor cursor, CXCursor /*parent*/,
                                                  CXClientData client_data)
{
    ReferenceData* data = static_cast<ReferenceData*>(client_data);
    CXString usr = clang_getCursorUSR(cursor);
    const char* str = clang_getCString(usr);
    if (!str || !strcmp(str, "")) {
        clang_disposeString(usr);
        usr = clang_getCursorUSR(clang_getCursorReferenced(cursor));
        str = clang_getCString(usr);
    }
    if (!strncmp(str, data->usr, data->usrlen)) {
        CXSourceLocation loc = clang_getCursorLocation(cursor);

        CXFile rfile;
        unsigned int rrow, rcol, roff;
        clang_getSpellingLocation(loc, &rfile, &rrow, &rcol, &roff);
        CXString filename = clang_getFileName(rfile);

        //qDebug() << "ref found" << clang_getCString(filename) << rrow << rcol << roff;
        QByteArray qfn(Path::resolved(clang_getCString(filename)));
        qfn += ":" + QByteArray::number(rrow)
            + ":" + QByteArray::number(rcol)
            + ":" + QByteArray::number(roff);
        data->results.insert(qfn);

        clang_disposeString(filename);
    }
    return CXChildVisit_Recurse;
}

static bool visitFindReferences(UnitCache::Unit* ud, void* data)
{
    CXCursor unitcursor = clang_getTranslationUnitCursor(ud->unit);
    clang_visitChildren(unitcursor, visitUnitFindReferences, data);
    return false;
}

struct FirstUnitData
{
    FirstUnitData() : data(0) { }
    ~FirstUnitData() { UnitCache::instance()->release(data); }

    QByteArray filename;
    UnitCache::Unit* data;
};

static bool visitFindFirstUnit(UnitCache::Unit* ud, void* data)
{
    FirstUnitData* firstdata = static_cast<FirstUnitData*>(data);

    CXFile file = clang_getFile(ud->unit, firstdata->filename.constData());
    if (!file)
        return false;

    firstdata->data = UnitCache::instance()->acquire(ud->filename); // add a new ref
    return true;
}

int DatabaseImpl::referencesForLocation(const QByteArray& filename, int row, int col)
{
    const int id = ++lastJobId;

    qDebug() << "references for location" << filename << Resource::hash(filename) << row << col;

    ReferencesJob* job = new ReferencesJob(filename, id, row, col);
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

int DatabaseImpl::recompile(const QByteArray& filename)
{
    const int id = ++lastJobId;

    qDebug() << "recompile" << filename;

    RecompileJob* job = new RecompileJob(filename, id);
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

static inline bool isCursorReference(CXCursorKind kind)
{
    return (kind >= CXCursor_FirstRef && kind <= CXCursor_LastRef);
}

int DatabaseImpl::followLocation(const QByteArray& filename, int row, int col)
{
    const int id = ++lastJobId;

    FollowLocationJob* job = new FollowLocationJob(filename, id, row, col);
    connect(job, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SIGNAL(complete(int, const QList<QByteArray>&)));
    QThreadPool::globalInstance()->start(job);

    return id;
}

FollowLocationJob::FollowLocationJob(const QByteArray&fn, int i, int l, int c)
    : filename(fn), id(i), line(l), col(c)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

static inline void debugCursor(CXCursor c)
{
    CXSourceLocation loc = clang_getCursorLocation(c);
    CXFile file;
    unsigned int line, col;
    clang_getSpellingLocation(loc, &file, &line, &col, 0);
    CXString fn = clang_getFileName(file);
    qDebug() << Path::resolved(clang_getCString(fn)) << line << col;
    clang_disposeString(fn);
}

void FollowLocationJob::run()
{
    CachedUnit locker(filename, UnitCache::AST);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.filename = filename;
        visitIncluderFiles(filename, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            qDebug("follow: no unit for %s", filename.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXFile file = clang_getFile(data->unit, filename.constData());
    CXSourceLocation loc = clang_getLocation(data->unit, file, line, col);
    CXCursor cursor = clang_getCursor(data->unit, loc);
    CXCursorKind cursorKind = clang_getCursorKind(cursor);
    CXCursor ref = clang_getCursorReferenced(cursor);
    CXCursorKind refKind = clang_getCursorKind(ref);
    qDebug() << "cursor" << cursorKind << "ref" << refKind;
    debugCursor(cursor);
    debugCursor(ref);
    qDebug() << "---";
    bool shouldFindDef = false;
    switch (refKind) {
    case CXCursor_StructDecl:
    case CXCursor_ClassDecl:
        shouldFindDef = true;
        break;
    default:
        break;
    }
    if (clang_equalCursors(cursor, ref) || shouldFindDef) {
        qDebug() << "I am myself or we should be on a definition";
        if (!clang_isCursorDefinition(ref)) {
            qDebug() << "not an outright definition";
            ref = clang_getCursorDefinition(cursor);
        }
        if (!clang_isCursorDefinition(ref)) {
            qDebug() << "no definition found, trying to read one from leveldb";
            CXString usr = clang_getCursorUSR(cursor);
            const char* cusr = clang_getCString(usr);
            if (!strlen(cusr)) {
                clang_disposeString(usr);
                usr = clang_getCursorUSR(ref);
                cusr = clang_getCString(usr);
                if (!strlen(cusr)) {
                    clang_disposeString(usr);
                    qDebug() << "no USR found, bailing out";
                    emit complete(id, QList<QByteArray>());
                    return;
                }
            }

            leveldb::DB* db = 0;
            QByteArray dbname = Database::databaseName(Database::Definition);
            leveldb::Status status = leveldb::DB::Open(leveldb::Options(), dbname.constData(), &db);
            if (!status.ok()) {
                qWarning("no definition db!");
                clang_disposeString(usr);
                emit complete(id, QList<QByteArray>());
                return;
            }
            std::string value;
            db->Get(leveldb::ReadOptions(), cusr, &value);
            clang_disposeString(usr);
            delete db;
            if (value.empty()) {
                qDebug() << "no definition resource found, bailing out";
                emit complete(id, QList<QByteArray>());
                return;
            }
            QByteArray bvalue = QByteArray::fromRawData(value.c_str(), value.size());
            QList<QByteArray> defs = bvalue.split('\n');
            emit complete(id, defs);
            return;
        }
    }
    loc = clang_getCursorLocation(ref);

    CXFile rfile;
    unsigned int rrow, rcol, roff;
    clang_getSpellingLocation(loc, &rfile, &rrow, &rcol, &roff);
    CXString unitfn = clang_getFileName(rfile);

    qDebug() << "followed to" << clang_getCString(unitfn) << rrow << rcol << roff;
    QByteArray qfn(Path::resolved(clang_getCString(unitfn)));
    qfn += ":" + QByteArray::number(rrow)
        + ":" + QByteArray::number(rcol)
        + ":" + QByteArray::number(roff);

    clang_disposeString(unitfn);

    emit complete(id, QList<QByteArray>() << qfn);
}

ReferencesJob::ReferencesJob(const QByteArray& fn, int i, int l, int c)
    : filename(fn), id(i), line(l), col(c)
{
}

ReferencesJob::~ReferencesJob()
{
}

static inline void readReferences(const char* usr, QList<QByteArray>& refs)
{
    QByteArray filename = Database::databaseName(Database::Reference);

    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), filename.constData(), &db);
    if (!status.ok())
        return;

    std::string value;
    db->Get(leveldb::ReadOptions(), usr, &value);
    if (value.empty()) {
        delete db;
        return;
    }

    QByteArray qvalue = QByteArray::fromRawData(value.c_str(), value.size());
    refs = qvalue.split('\n');

    delete db;
}

void ReferencesJob::run()
{
    if (line == -1 && col == -1)
        runName();
    else
        runLocation();
}

void ReferencesJob::runName()
{
    QByteArray databasename = Database::databaseName(Database::Symbol);

    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), databasename.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    std::string value;
    db->Get(leveldb::ReadOptions(), filename.constData(), &value);
    if (value.empty()) {
        delete db;
        emit complete(id, QList<QByteArray>());
        return;
    }

    QByteArray bvalue = QByteArray::fromRawData(value.c_str(), value.size());
    QList<QByteArray> list = bvalue.split('\n');
    //qDebug() << "runName result" << list;

    delete db;

    databasename = Database::databaseName(Database::Definition);
    status = leveldb::DB::Open(leveldb::Options(), databasename.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    QList<QByteArray> result;
    foreach(const QByteArray& entry, list) {
        if (entry.isEmpty())
            continue;
        // if the entry looks like a path, return it outright
        if (entry.at(0) == '/') {
            result.append(entry);
            continue;
        }
        db->Get(leveldb::ReadOptions(), entry.constData(), &value);
        if (value.empty())
            continue;
        bvalue = QByteArray::fromRawData(value.c_str(), value.size());
        foreach(const QByteArray& line, bvalue.split('\n')) {
            if (!line.isEmpty())
                result.append(line);
        }
    }

    delete db;
    emit complete(id, result);
}

void ReferencesJob::runLocation()
{
    CachedUnit locker(filename, UnitCache::AST);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.filename = filename;
        visitIncluderFiles(filename, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            qDebug("references: no unit for %s", filename.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXTranslationUnit unit = data->unit;
    CXFile file = data->file;

    CXSourceLocation loc = clang_getLocation(unit, file, line, col);
    CXCursor cursor = clang_getCursor(unit, loc);
    CXCursor def = clang_getCursorDefinition(cursor);
    if (clang_equalCursors(def, clang_getNullCursor())) {
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (clang_equalCursors(ref, clang_getNullCursor())) {
            qDebug("no ref and no def!");
            emit complete(id, QList<QByteArray>());
            return;
        }

        CXString usr = clang_getCursorUSR(ref);
        if (!clang_getCString(usr) || !strcmp(clang_getCString(usr), "")) {
            clang_disposeString(usr);
            qDebug() << "no usr";
            emit complete(id, QList<QByteArray>());
            return;
        }

        QList<QByteArray> refs;
        readReferences(clang_getCString(usr), refs);
        emit complete(id, refs);
        return;
    }

    CXString usr = clang_getCursorUSR(def);
    if (!clang_getCString(usr) || !strcmp(clang_getCString(usr), "")) {
        clang_disposeString(usr);
        qDebug() << "no usr";
        emit complete(id, QList<QByteArray>());
        return;
    }

    QList<QByteArray> refs;
    readReferences(clang_getCString(usr), refs);
    clang_disposeString(usr);
    emit complete(id, refs);
}

RecompileJob::RecompileJob(const QByteArray& fn, int i)
    : filename(fn), id(i)
{
}

RecompileJob::~RecompileJob()
{
}

void RecompileJob::run()
{
    CachedUnit locker(filename, UnitCache::AST);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.filename = filename;
        visitIncluderFiles(filename, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            qDebug("recompile: no unit for %s", filename.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }
    UnitCache::instance()->recompile(data);

    emit complete(id, QList<QByteArray>());
}

MatchJob::MatchJob(const QByteArray& p, int i)
    : partial(p), id(i)
{
}

MatchJob::~MatchJob()
{
}

void MatchJob::run()
{
    QByteArray databasename = Database::databaseName(Database::Symbol);

    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), databasename.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    QList<QByteArray> result;

    QByteArray entry;
    leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
    it->Seek(partial.constData());
    while (it->Valid()) {
        entry = it->key().ToString().c_str();
        /*if ((entry.contains('(') && !partial.contains('('))
            || (entry.contains(':') && !partial.contains(':'))) {
            it->Next();
            continue;
        }*/
        if (entry.startsWith(partial))
            result.append(entry);
        else
            break;
        it->Next();
    }
    delete it;

    delete db;

    emit complete(id, result);
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

static inline bool makeLocation(QByteArray& query, int* line, int* col)
{
    bool ok;
    int pos = query.lastIndexOf(':');
    if (pos == -1)
        return false;
    *col = query.mid(pos + 1).toInt(&ok);
    if (!ok)
        return false;
    query = query.left(pos);

    pos = query.lastIndexOf(':');
    if (pos == -1)
        return false;
    *line = query.mid(pos + 1).toInt(&ok);
    if (!ok)
        return false;
    query = query.left(pos);
    return true;
}

int Database::followLocation(const QByteArray& query)
{
    int row, col;
    QByteArray filename = query;
    if (!makeLocation(filename, &row, &col))
        return -1;

    return m_impl->followLocation(filename, row, col);
}

int Database::referencesForLocation(const QByteArray& query)
{
    int row, col;
    QByteArray filename = query;
    if (!makeLocation(filename, &row, &col))
        return -1;

    return m_impl->referencesForLocation(filename, row, col);
}

int Database::referencesForName(const QByteArray& name)
{
    return m_impl->referencesForName(name);
}

int Database::recompile(const QByteArray& query)
{
    return m_impl->recompile(query);
}

int Database::match(const QByteArray& query)
{
    return m_impl->match(query);
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
