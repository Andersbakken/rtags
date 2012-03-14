#include "Indexer.h"
#include "Shared.h"
#include "UnitCache.h"
#include "Resource.h"
#include "Path.h"
#include "Database.h"
#include "leveldb/db.h"
#include "leveldb/write_batch.h"
#include <QElapsedTimer>
#include <QHash>
#include <QThread>
#include <QThreadPool>
#include <QRunnable>
#include <QWaitCondition>
#include <QMutex>
#include <QMutexLocker>
#include <QSet>
#include <QVector>
#include <QDir>
#include <Log.h>

#define SYNCINTERVAL 10

class IndexerJob;

typedef QHash<QByteArray, QSet<QByteArray> > HashSet;

class IndexerSyncer : public QThread
{
public:
    IndexerSyncer(QObject* parent = 0);

    void addSet(Database::Type type, const HashSet& set);
    void notify();
    void stop();

protected:
    void run();

private:
    void unite(HashSet& set, const HashSet& other);
    void writeSet(Database::Type type, HashSet& data);

private:
    bool stopped;
    QMutex mutex;
    QWaitCondition cond;
    HashSet incs, defs, refs, syms;
};

IndexerSyncer::IndexerSyncer(QObject* parent)
    : QThread(parent), stopped(false)
{
}

inline void IndexerSyncer::unite(HashSet& set, const HashSet& other)
{
    HashSet::const_iterator it = other.begin();
    const HashSet::const_iterator end = other.end();
    while (it != end) {
        set[it.key()].unite(other.value(it.key()));
        ++it;
    }
}

void IndexerSyncer::stop()
{
    QMutexLocker locker(&mutex);
    stopped = true;
    cond.wakeOne();
}

void IndexerSyncer::notify()
{
    QMutexLocker locker(&mutex); // is this needed here?
    cond.wakeOne();
}

void IndexerSyncer::addSet(Database::Type type, const HashSet& set)
{
    QMutexLocker locker(&mutex);
    switch (type) {
    case Database::Include:
        unite(incs, set);
        break;
    case Database::Definition:
        unite(defs, set);
        break;
    case Database::Reference:
        unite(refs, set);
        break;
    case Database::Symbol:
        unite(syms, set);
        break;
    }
    //cond.wakeOne();
}

void IndexerSyncer::writeSet(Database::Type type, HashSet& data)
{
    leveldb::DB* db = 0;
    leveldb::Options options;
    options.create_if_missing = true;
    QByteArray name = Database::databaseName(type);
    if (name.isEmpty())
        return;

    leveldb::Status status = leveldb::DB::Open(options, name.constData(), &db);
    if (!status.ok())
        return;
    Q_ASSERT(db);

    leveldb::WriteBatch batch;
    const leveldb::ReadOptions readopts;

    HashSet::iterator it = data.begin();
    const HashSet::const_iterator end = data.end();
    while (it != end) {
        QSet<QByteArray>& set = it.value();

        std::string value;
        db->Get(readopts, it.key().constData(), &value);

        QByteArray bvalue = QByteArray::fromRawData(value.c_str(), value.size());
        QSet<QByteArray> newset = bvalue.split('\n').toSet(), inter;
        newset.remove(QByteArray(""));

        inter = newset & set; // intersection
        if (inter.size() == set.size()) { // if the intersection contains all of our preexisting items then we're good
            ++it;
            continue;
        }
        newset.unite(set);

        value.clear();
        QSet<QByteArray>::const_iterator vit = newset.begin();
        const QSet<QByteArray>::const_iterator vend = newset.end();
        while (vit != vend) {
            value += (*vit).constData();
            value += '\n';
            ++vit;
        }

        batch.Put(it.key().constData(), value);
        ++it;
    }
    data.clear();

    db->Write(leveldb::WriteOptions(), &batch);
    delete db;
}

void IndexerSyncer::run()
{
    QMutexLocker locker(&mutex);
    while (!stopped) {
        cond.wait(&mutex, 10000);
        log(1) << "syncing";
        if (!incs.isEmpty()) {
            writeSet(Database::Include, incs);
        }
        if (!defs.isEmpty()) {
            writeSet(Database::Definition, defs);
        }
        if (!refs.isEmpty()) {
            writeSet(Database::Reference, refs);
        }
        if (!syms.isEmpty()) {
            writeSet(Database::Symbol, syms);
        }
        log(1) << "synced";
    }
}

class IndexerImpl
{
public:
    int jobCounter;

    QMutex implMutex;
    QWaitCondition implCond;
    QSet<QByteArray> indexing;

    QByteArray path;
    int lastJobId;
    QHash<int, IndexerJob*> jobs;

    IndexerSyncer* syncer;

    QMutex incMutex;
    HashSet incs;

    bool timerRunning;
    QElapsedTimer timer;

    QMutex resolveMutex;
    QHash<QByteArray, QByteArray> resolveCache;
};

class IndexerJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    IndexerJob(IndexerImpl* impl, Indexer::Type type, Indexer::Mode mode, int id,
               const QByteArray& path, const QByteArray& input, const QByteArray& output,
               const QList<QByteArray>& arguments);

    int id() const { return m_id; }

    void run();

    Indexer::Type m_type;
    Indexer::Mode m_mode;
    int m_id;
    QByteArray m_path, m_in, m_out;
    QList<QByteArray> m_args;
    IndexerImpl* m_impl;

    HashSet m_defs, m_refs, m_syms;

private:
    void addFileNameSymbol(const QByteArray& fileName);

signals:
    void done(int id, const QByteArray& input, const QByteArray& output);
};

#include "Indexer.moc"

static inline void addInclusion(IndexerJob* job, CXFile inc)
{
    CXString str = clang_getFileName(inc);

    const QByteArray path = Path::resolved(clang_getCString(str));

    QMutexLocker locker(&job->m_impl->incMutex);
    if (!qstrcmp(job->m_in, path)) {
        clang_disposeString(str);
        return;
    }

    job->m_impl->incs[path].insert(job->m_in);
    clang_disposeString(str);
}

static void inclusionVisitor(CXFile included_file,
                             CXSourceLocation*,
                             unsigned include_len,
                             CXClientData client_data)
{
    IndexerJob* job = static_cast<IndexerJob*>(client_data);
    if (include_len)
        addInclusion(job, included_file);
}

static inline void addNamePermutations(CXCursor cursor, const char* usr, IndexerJob* job)
{
    QByteArray qusr = QByteArray(usr), qname;
    QByteArray qparam, qnoparam;

    CXString displayName;
    CXCursor cur = cursor, null = clang_getNullCursor();
    CXCursorKind kind;
    for (;;) {
        if (clang_equalCursors(cur, null))
            break;
        kind = clang_getCursorKind(cur);
        if (clang_isTranslationUnit(kind))
            break;

        displayName = clang_getCursorDisplayName(cur);
        const char* name = clang_getCString(displayName);
        if (!name || !strlen(name)) {
            clang_disposeString(displayName);
            break;
        }
        qname = QByteArray(name);
        if (qparam.isEmpty()) {
            qparam.prepend(qname);
            qnoparam.prepend(qname);
            const int sp = qnoparam.indexOf('(');
            if (sp != -1)
                qnoparam = qnoparam.left(sp);
        } else {
            qparam.prepend(qname + "::");
            qnoparam.prepend(qname + "::");
        }
        job->m_syms[qparam].insert(qusr);
        if (qparam != qnoparam)
            job->m_syms[qnoparam].insert(qusr);

        clang_disposeString(displayName);
        cur = clang_getCursorSemanticParent(cur);
    }
}

static CXChildVisitResult indexVisitor(CXCursor cursor,
                                       CXCursor /*parent*/,
                                       CXClientData client_data)
{
    IndexerJob* job = static_cast<IndexerJob*>(client_data);

    CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_CXXAccessSpecifier:
        return CXChildVisit_Recurse;
    default:
        break;
    }

    CXString usr = clang_getCursorUSR(cursor);
    const char* cusr = clang_getCString(usr);
    int usrlen = strlen(cusr);
    if (!usrlen || (usrlen == 2 && !strncmp(cusr, "c:", 2))) {
        clang_disposeString(usr);
        CXCursor ref = clang_getCursorReferenced(cursor);
        usr = clang_getCursorUSR(ref);
        cusr = clang_getCString(usr);
        usrlen = strlen(cusr);
        if (!usrlen || (usrlen == 2 && !strncmp(cusr, "c:", 2))) {
            clang_disposeString(usr);
            return CXChildVisit_Recurse;
        }
    }
    //CXString kindstr = clang_getCursorKindSpelling(kind);

    CXSourceLocation loc = clang_getCursorLocation(cursor);
    CXFile file;
    unsigned int line, col;
    clang_getSpellingLocation(loc, &file, &line, &col, 0);
    CXString fileName = clang_getFileName(file);
    const char* cfileName = clang_getCString(fileName);
    if (!cfileName || !strlen(cfileName)) {
        //clang_disposeString(kindstr);
        clang_disposeString(usr);
        clang_disposeString(fileName);
        return CXChildVisit_Recurse;
    }
    QByteArray qloc;
    {
        QMutexLocker locker(&job->m_impl->resolveMutex);
        QHash<QByteArray, QByteArray>& cache = job->m_impl->resolveCache;
        QByteArray key = cfileName;
        QByteArray &val = cache[key];
        if (val.isEmpty()) {
            qloc = key;
            const int ret = canonicalizePath(qloc.data(), qloc.size());
            const int extra = digits(line) + digits(col) + 2;
            qloc.resize(ret + extra);
            snprintf(qloc.data() + ret, extra + 1, ":%d:%d", line, col);
            val = qloc;
        } else {
            qloc = val;
        }
    }

    if (clang_isCursorDefinition(cursor)
        || kind == CXCursor_FunctionDecl) {
        job->m_defs[cusr].insert(qloc);
        addNamePermutations(cursor, cusr, job);
    }
    job->m_refs[cusr].insert(qloc);

    Q_ASSERT(strcmp(cusr, "") != 0);
    Q_ASSERT(strcmp(cusr, "c:") != 0);
    //clang_disposeString(kindstr);
    clang_disposeString(fileName);
    clang_disposeString(usr);

    return CXChildVisit_Recurse;
}

IndexerJob::IndexerJob(IndexerImpl* impl, Indexer::Type type, Indexer::Mode mode, int id,
                       const QByteArray& path, const QByteArray& input, const QByteArray& output,
                       const QList<QByteArray>& arguments)
    : m_type(type), m_mode(mode), m_id(id), m_path(path), m_in(input), m_out(output), m_args(arguments), m_impl(impl)
{
}

inline void IndexerJob::addFileNameSymbol(const QByteArray& fileName)
{
    // ### would it be faster/better to use QFileInfo here?
    int idx = -1;
    for (;;) {
        int backslashes = 0;
        idx = fileName.lastIndexOf('/', idx);
        while (idx > 0 && fileName.at(idx - 1) == '\\') {
            ++backslashes;
            --idx;
        }
        if ((backslashes % 2) || !idx) {
            idx -= 1;
            if (!idx)
                break;
        } else {
            idx += backslashes;
            break;
        }
    }
    if (idx == -1)
        return;
    m_syms[fileName.mid(idx + 1)].insert(fileName + ":1:1");
}

static inline void uniteSets(HashSet& dst, HashSet& src)
{
    HashSet::const_iterator it = src.begin();
    const HashSet::const_iterator end = src.end();
    while (it != end) {
        dst[it.key()].unite(it.value());
        ++it;
    }
    src.clear();
}

static inline QList<QByteArray> extractPchFiles(const QList<QByteArray>& args)
{
    QList<QByteArray> out;
    bool nextIsPch = false;
    foreach(const QByteArray& arg, args) {
        if (arg.isEmpty())
            continue;

        if (nextIsPch) {
            nextIsPch = false;
            out.append(arg);
        } else if (arg == "-include-pch")
            nextIsPch = true;
    }
    return out;
}

void IndexerJob::run()
{
    int unitMode = UnitCache::Source | UnitCache::AST;
    if (m_mode != Indexer::Force)
        unitMode |= UnitCache::Memory;

    QList<QByteArray> pchFiles = extractPchFiles(m_args);
    if (!pchFiles.isEmpty()) {
        QMutexLocker locker(&m_impl->implMutex);
        bool wait;
        do {
            wait = false;
            foreach(const QByteArray& pchFile, pchFiles) {
                if (m_impl->indexing.contains(pchFile)) {
                    wait = true;
                    break;
                }
            }
            if (wait)
                m_impl->implCond.wait(&m_impl->implMutex);
        } while (wait);
    }

    // ### hack for now
    CachedUnit unit(m_in, m_in, m_args, unitMode);

    if (unit.unit()) {
        log(1) << "parsing" << m_in << unit.unit()->fileName;
        CXTranslationUnit tu = unit.unit()->unit;
        unsigned int diagCount = clang_getNumDiagnostics(tu);
        for (unsigned int i = 0; i < diagCount; ++i) {
            const CXDiagnostic diag = clang_getDiagnostic(tu, i);
            const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diag);
            if (severity >= CXDiagnostic_Warning) {
                CXString msg = clang_formatDiagnostic(diag, CXDiagnostic_DisplaySourceLocation
                                                      | CXDiagnostic_DisplayColumn
                                                      | CXDiagnostic_DisplayOption
                                                      | CXDiagnostic_DisplayCategoryName);
                warning("clang: %s", clang_getCString(msg));
                clang_disposeString(msg);
            }
            clang_disposeDiagnostic(diag);
        }

        if (unit.unit()->origin == UnitCache::Source) {
            log(1) << "reread" << unit.unit()->fileName << "from source, revisiting";
            clang_getInclusions(tu, inclusionVisitor, this);
            clang_visitChildren(clang_getTranslationUnitCursor(tu), indexVisitor, this);
            addFileNameSymbol(unit.unit()->fileName);

            m_impl->syncer->addSet(Database::Definition, m_defs);
            m_defs.clear();

            m_impl->syncer->addSet(Database::Reference, m_refs);
            m_refs.clear();

            m_impl->syncer->addSet(Database::Symbol, m_syms);
            m_syms.clear();
        }
    } else {
        log(1) << "got 0 unit for" << m_in;
    }

    emit done(m_id, m_in, m_out);
}

Indexer* Indexer::s_inst = 0;

Indexer::Indexer(const QByteArray& path, QObject* parent)
    : QObject(parent), m_impl(new IndexerImpl)
{
    Q_ASSERT(path.startsWith('/'));
    if (!path.startsWith('/'))
        return;
    QDir dir;
    dir.mkpath(path);

    m_impl->jobCounter = 0;
    m_impl->lastJobId = 0;
    m_impl->path = path;
    m_impl->timerRunning = false;
    m_impl->syncer = new IndexerSyncer(this);
    m_impl->syncer->start();

    s_inst = this;
}

Indexer::~Indexer()
{
    s_inst = 0;
    m_impl->syncer->stop();
    m_impl->syncer->wait();

    delete m_impl;
}

Indexer* Indexer::instance()
{
    return s_inst;
}

int Indexer::index(Type type, const QByteArray& input, const QByteArray& output,
                   const QList<QByteArray>& arguments, Mode mode)
{
    QMutexLocker locker(&m_impl->implMutex);

    if (m_impl->indexing.contains(output))
        return -1;

    int id;
    do {
        id = m_impl->lastJobId++;
    } while (m_impl->jobs.contains(id));

    m_impl->indexing.insert(output);

    IndexerJob* job = new IndexerJob(m_impl, type, mode, id, m_impl->path, input, output, arguments);
    m_impl->jobs[id] = job;
    connect(job, SIGNAL(done(int, const QByteArray&, const QByteArray&)),
            this, SLOT(jobDone(int, const QByteArray&, const QByteArray&)), Qt::QueuedConnection);

    if (!m_impl->timerRunning) {
        m_impl->timerRunning = true;
        m_impl->timer.restart();
    }

    QThreadPool::globalInstance()->start(job);

    return id;
}

void Indexer::jobDone(int id, const QByteArray& input, const QByteArray& output)
{
    Q_UNUSED(input)

    QMutexLocker locker(&m_impl->implMutex);
    m_impl->jobs.remove(id);
    if (m_impl->indexing.remove(output))
        m_impl->implCond.wakeAll();

    ++m_impl->jobCounter;

    if (m_impl->jobs.isEmpty() || m_impl->jobCounter == SYNCINTERVAL) {
        {
            QMutexLocker inclocker(&m_impl->incMutex);
            m_impl->syncer->addSet(Database::Include, m_impl->incs);
            m_impl->incs.clear();
        }
        m_impl->jobCounter = 0;

        if (m_impl->jobs.isEmpty()) {
            m_impl->syncer->notify();

            Q_ASSERT(m_impl->timerRunning);
            m_impl->timerRunning = false;
            log(1) << "jobs took" << m_impl->timer.elapsed() << "ms";
        }
    }

    emit indexingDone(id);
}
