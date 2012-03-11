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

class DumpJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    DumpJob(const QByteArray& fn, int i);
signals:
    void complete(int id, const QList<QByteArray>& locations);
protected:
    void run();
private:
    QByteArray fileName;
    int id;
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
    QByteArray fileName;
    int id, line, col;
};

class CursorInfoJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    CursorInfoJob(const QByteArray& fn, int i, int l, int c);
    ~CursorInfoJob();

signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();

private:
    QByteArray fileName;
    int id, line, col;
};

class CodeCompleteJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    CodeCompleteJob(const QByteArray& fn, int i, int l, int c, const QHash<Path, QByteArray> &unsaved);
    ~CodeCompleteJob();

signals:
    void complete(int id, const QList<QByteArray>& output);

protected:
    void run();

private:
    QByteArray fileName;
    int id, line, col;
    QHash<Path, QByteArray> unsavedFiles;
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
    QByteArray fileName;
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
    QByteArray fileName;
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

struct DumpUserData {
    QList<QByteArray> lines;
    int indent;
};

static CXChildVisitResult dumpVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    DumpUserData *dump = reinterpret_cast<DumpUserData*>(userData);
    QByteArray line(dump->indent * 2, ' ');
    line += cursorToString(cursor);
    CXCursor ref = clang_getCursorReferenced(cursor);
    if (!clang_equalCursors(cursor, ref) && !clang_isInvalid(clang_getCursorKind(ref))) {
        line += " => " + cursorToString(ref);
    }
    dump->lines.append(line);
    ++dump->indent;
    clang_visitChildren(cursor, dumpVisitor, dump);
    --dump->indent;
    return CXChildVisit_Continue;
}

typedef bool (*VisitFile)(UnitCache::Unit* unit, void* data);

static void visitIncluderFiles(const QByteArray& fileName, VisitFile visitor, void* data,
                               int mode = UnitCache::AST | UnitCache::Memory)
{
    qDebug() << "looking at" << fileName;

    QByteArray dbname = Database::databaseName(Database::Include);
    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), dbname.constData(), &db);
    if (!status.ok())
        return;
    std::string value;
    db->Get(leveldb::ReadOptions(), fileName.constData(), &value);
    delete db;
    if (value.empty())
        return;

    QByteArray bvalue = QByteArray::fromRawData(value.c_str(), value.size());
    QList<QByteArray> others = bvalue.split('\n');
    foreach(const QByteArray& inc, others) {
        if (inc.isEmpty())
            continue;
        qDebug() << "about to visit" << inc;

        CachedUnit unit(inc, mode);
        if (unit.unit()) {
            if ((*visitor)(unit.unit(), data))
                break;
        } else
            qWarning("Unit not found: %s", inc.constData());
    }
}

struct FirstUnitData
{
    FirstUnitData() : data(0) { }
    ~FirstUnitData() { UnitCache::instance()->release(data); }

    QByteArray fileName;
    UnitCache::Unit* data;
};

static bool visitFindFirstUnit(UnitCache::Unit* ud, void* data)
{
    FirstUnitData* firstdata = static_cast<FirstUnitData*>(data);

    CXFile file = clang_getFile(ud->unit, firstdata->fileName.constData());
    if (!file)
        return false;

    firstdata->data = UnitCache::instance()->acquire(ud->fileName); // add a new ref
    return true;
}

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
    printf("%s:%d %s\n", __FILE__, __LINE__, __FUNCTION__);
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

FollowLocationJob::FollowLocationJob(const QByteArray&fn, int i, int l, int c)
    : fileName(fn), id(i), line(l), col(c)
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
    CachedUnit locker(fileName, UnitCache::AST | UnitCache::Memory);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.fileName = fileName;
        visitIncluderFiles(fileName, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            qDebug("follow: no unit for %s", fileName.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXFile file = clang_getFile(data->unit, fileName.constData());
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

CursorInfoJob::CursorInfoJob(const QByteArray&fn, int i, int l, int c)
    : fileName(fn), id(i), line(l), col(c)
{
}

CursorInfoJob::~CursorInfoJob()
{
}

static CXChildVisitResult memberVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_CXXMethod:
    case CXCursor_FieldDecl:
        reinterpret_cast<QList<QByteArray> *>(userData)->append(eatString(clang_getCursorSpelling(cursor)));
        break;
    default:
        break;
    }
    return CXChildVisit_Continue;
}

static inline bool hasMembers(CXCursor cursor)
{
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
        return true;
    default:
        break;
    }
    return false;
}

void CursorInfoJob::run()
{
    CachedUnit locker(fileName, UnitCache::AST | UnitCache::Memory);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.fileName = fileName;
        visitIncluderFiles(fileName, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            qDebug("cursorInfo: no unit for %s", fileName.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXFile file = clang_getFile(data->unit, fileName.constData());
    CXSourceLocation loc = clang_getLocation(data->unit, file, line, col);
    CXCursor cursor = clang_getCursor(data->unit, loc);
    QList<QByteArray> ret;
    if (!clang_isInvalid(clang_getCursorKind(cursor))) {
        bool go = hasMembers(cursor);
        if (!go) {
            cursor = clang_getCursorReferenced(cursor);
            go = hasMembers(cursor);
        }
        if (go)
            clang_visitChildren(cursor, memberVisitor, &ret);
    }
    emit complete(id, ret);
}

CodeCompleteJob::CodeCompleteJob(const QByteArray&fn, int i, int l, int c,
                                 const QHash<Path, QByteArray> &unsaved)
    : fileName(fn), id(i), line(l), col(c), unsavedFiles(unsaved)
{
}

CodeCompleteJob::~CodeCompleteJob()
{
}

static inline bool isOperator(const char *str, int len)
{
    if (len >= 9 && !strncmp(str, "operator", 8)) {
        const char ch = str[8];
        if (!isalnum(ch) && ch != '_')
            return true;
    }
    return false;
}
void CodeCompleteJob::run()
{
    CachedUnit locker(fileName, UnitCache::AST | UnitCache::Memory | UnitCache::Source);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.fileName = fileName;
        visitIncluderFiles(fileName, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            qDebug("codecomplete: no unit for %s", fileName.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXUnsavedFile *unsaved = 0;
    if (unsavedFiles.size()) {
        unsaved = new CXUnsavedFile[unsavedFiles.size()];
        int i = 0;
        for (QHash<Path, QByteArray>::const_iterator it = unsavedFiles.begin(); it != unsavedFiles.end(); ++it) {
            unsaved[i].Filename = it.key().constData();
            unsaved[i].Contents = it.value().constData();
            unsaved[i].Length = it.value().size();
        }
    }
    CXCodeCompleteResults *results = clang_codeCompleteAt(data->unit,
                                                          data->fileName.constData(),
                                                          line, col,
                                                          unsaved, unsavedFiles.size(),
                                                          clang_defaultCodeCompleteOptions());
    if (unsaved)
        delete[] unsaved;

    qDebug() << results << results->NumResults << unsavedFiles.keys();
    QMap<QByteArray, bool> ret;
    for (unsigned int i = 0; i < results->NumResults; ++i) {
        const CXCompletionString& str = results->Results[i].CompletionString;
        if (clang_getCompletionAvailability(str) != CXAvailability_Available)
            continue;
        // qDebug() << "stuff" << i << clang_getCompletionPriority(str);
        // for (int b=0; b<clang_getCompletionNumAnnotations(str); ++b) {
        //     qDebug() << b << eatString(clang_getCompletionAnnotation(str, b));
        // }

        switch (results->Results[i].CursorKind) {
        case CXCursor_Destructor:
        case CXCursor_ClassDecl:
            continue;
        default:
            break;
        }
        // printf("Got thing %s %d\n" , eatString(clang_getCursorKindSpelling(results->Results[i].CursorKind)).constData(),
        //        clang_getNumCompletionChunks(str));

        for (unsigned int j = 0; j < clang_getNumCompletionChunks(str); ++j) {
            if (clang_getCompletionChunkKind(str, j) != CXCompletionChunk_TypedText)
                continue;

            CXString out = clang_getCompletionChunkText(str, j);
            const char *str = clang_getCString(out);
            const int len = strlen(str);
            if (!isOperator(str, len)) {
                ret[QByteArray(str, len)] = true;
            }
            clang_disposeString(out);
        }
    }

    clang_disposeCodeCompleteResults(results);
    emit complete(id, ret.keys());
}


ReferencesJob::ReferencesJob(const QByteArray& fn, int i, int l, int c)
    : fileName(fn), id(i), line(l), col(c)
{
}

ReferencesJob::~ReferencesJob()
{
}

static inline void readReferences(const char* usr, QList<QByteArray>& refs)
{
    QByteArray fileName = Database::databaseName(Database::Reference);

    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), fileName.constData(), &db);
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
    db->Get(leveldb::ReadOptions(), fileName.constData(), &value);
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
    CachedUnit locker(fileName, UnitCache::AST | UnitCache::Memory);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.fileName = fileName;
        visitIncluderFiles(fileName, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            qDebug("references: no unit for %s", fileName.constData());
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
    : fileName(fn), id(i)
{
}

RecompileJob::~RecompileJob()
{
}

void RecompileJob::run()
{
    CachedUnit locker(fileName, UnitCache::Source | UnitCache::Info);
    UnitCache::Unit* data = locker.unit();
    if (data) {
        // everything is good!
        emit complete(id, QList<QByteArray>());
        return;
    }

    FirstUnitData first;
    first.fileName = fileName;
    visitIncluderFiles(fileName, visitFindFirstUnit, &first, UnitCache::Source | UnitCache::Info);
    if (!first.data)
        qDebug("recompile: no unit for %s", fileName.constData());

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

DumpJob::DumpJob(const QByteArray& fn, int i)
    : fileName(fn), id(i)
{
}

void DumpJob::run()
{
    DumpUserData user = { QList<QByteArray>(), 0 };
    CachedUnit unit(fileName);
    if (unit.unit()) {
        clang_visitChildren(clang_getTranslationUnitCursor(unit.unit()->unit), dumpVisitor, &user);
        foreach(const QByteArray &line, user.lines) {
            fprintf(stderr, "%s\n", line.constData());
        }
    }
    emit complete(id, user.lines);
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
