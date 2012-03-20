#include "Indexer.h"
#include "RTags.h"
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
#include <QVarLengthArray>

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
    IndexerJob(IndexerImpl* impl, int id,
               const QByteArray& path, const QByteArray& input,
               const QList<QByteArray>& arguments);

    int id() const { return m_id; }

    void run();

    int m_id;
    struct CursorInfo {
        CursorInfo() : symbolLength(0) {}
        bool isNull() const { return symbolLength; }
        int symbolLength;
        RTags::Location target;
        // QSet<RTags::Location> references;
    };
    RTags::Location createLocation(CXCursor cursor, unsigned *len = 0) const;
    void addNamePermutations(CXCursor cursor);

    QHash<RTags::Location, CursorInfo> m_cursorInfo;
    QByteArray m_path, m_in;
    QList<QByteArray> m_args;
    IndexerImpl* m_impl;
    mutable RTags::Location m_lastLocation;
    mutable CXSourceRange m_lastSourceRange;
    mutable unsigned m_lastLocationLength;

private:
    void addFileNameSymbol(const QByteArray& fileName);

signals:
    void done(int id, const QByteArray& input);
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

void IndexerJob::addNamePermutations(CXCursor cursor)
{
//     CXString displayName;
//     CXCursor null = clang_getNullCursor();
//     CXCursorKind kind;
//     while (true) {
//         if (clang_equalCursors(cursor, null))
//             break;
//         kind = clang_getCursorKind(cursor);
//         if (clang_isTranslationUnit(kind))
//             break;

//         displayName = clang_getCursorDisplayName(cursor);
//         const char* name = clang_getCString(displayName);
//         if (!name || !strlen(name)) {
//             clang_disposeString(displayName);
//             break;
//         }
//         QByteArray qname = QByteArray(name);
//         if (qparam.isEmpty()) {
//             qparam.prepend(qname);
//             qnoparam.prepend(qname);
//             const int sp = qnoparam.indexOf('(');
//             if (sp != -1)
//                 qnoparam = qnoparam.left(sp);
//         } else {
//             qparam.prepend(qname + "::");
//             qnoparam.prepend(qname + "::");
//         }
//         job->m_syms[qparam].insert(qusr);
//         if (qparam != qnoparam)
//             job->m_syms[qnoparam].insert(qusr);

//         clang_disposeString(displayName);
//         cur = clang_getCursorSemanticParent(cur);
//     }
}

RTags::Location IndexerJob::createLocation(CXCursor cursor, unsigned *len) const
{
    CXSourceRange range = clang_getCursorExtent(cursor);
    if (len && clang_equalRanges(range, m_lastSourceRange)) {
        *len = m_lastLocationLength;
        return m_lastLocation;
    }

    RTags::Location ret;
    if (!clang_Range_isNull(range)) {
        CXFile file;
        unsigned start;
        clang_getSpellingLocation(clang_getRangeStart(range), &file, 0, 0, &start);
        ret.offset = start;
        CXString fn = clang_getFileName(file);
        ret.path = clang_getCString(fn);
        ret.path.canonicalizePath(); // ### could canonicalize directly
        clang_disposeString(fn);
        if (len) { // only cache the last one when (len != 0)
            unsigned end;
            clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);

            m_lastLocationLength = end - start;
            *len = m_lastLocationLength;
            m_lastLocation = ret;
        }
        // unsigned l, c;
        // clang_getSpellingLocation(clang_getRangeStart(range), 0, &l, &c, 0);
        // QByteArray out;
        // out.append(ret.path);
        // out.append(':');
        // out.append(QByteArray::number(l));
        // out.append(':');
        // out.append(QByteArray::number(c));
        // debug() << ret.key() << "is" << out;
    } else if (len) {
        *len = 0;
    }
    return ret;
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

    CXCursor ref = clang_getCursorReferenced(cursor);
    CXCursorKind refKind = clang_getCursorKind(ref);
    if (!clang_isInvalid(refKind) && !clang_equalCursors(cursor, ref)) {
        unsigned len = 0;
        const RTags::Location loc = job->createLocation(cursor, &len);
        if (loc.path.isEmpty() || job->m_cursorInfo.contains(loc))
            return CXChildVisit_Recurse;
        if (clang_isCursorDefinition(cursor)
            || kind == CXCursor_FunctionDecl) {
            // job->m_defs[cusr].insert(qloc);
            job->addNamePermutations(cursor);
        }

        const RTags::Location refLoc = job->createLocation(ref);
        if (refLoc.path.isEmpty())
            return CXChildVisit_Recurse;
        IndexerJob::CursorInfo &info = job->m_cursorInfo[loc];
        info.symbolLength = len;
        info.target = refLoc;
    }
    return CXChildVisit_Recurse;

}

IndexerJob::IndexerJob(IndexerImpl* impl, int id,
                       const QByteArray& path, const QByteArray& input,
                       const QList<QByteArray>& arguments)
    : m_id(id), m_path(path), m_in(input), m_args(arguments), m_impl(impl),
    m_lastSourceRange(clang_getNullRange()), m_lastLocationLength(0)
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
    // m_syms[fileName.mid(idx + 1)].insert(fileName + ":1:1");
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
    QList<QByteArray> pchFiles = extractPchFiles(m_args);
    if (!pchFiles.isEmpty()) {
        QMutexLocker locker(&m_impl->implMutex);
        bool wait;
        do {
            wait = false;
            foreach (const QByteArray& pchFile, pchFiles) {
                if (m_impl->indexing.contains(pchFile)) {
                    wait = true;
                    break;
                }
            }
            if (wait) {
                m_impl->implCond.wait(&m_impl->implMutex);
            }
        } while (wait);
    }

    QVarLengthArray<const char*, 32> clangArgs(m_args.size());
    QByteArray clangLine = "clang ";
    bool nextIsPch = false;

    int idx = 0;
    foreach(const QByteArray& arg, m_args) {
        if (arg.isEmpty())
            continue;
        if (nextIsPch) {
            nextIsPch = false;
            Resource resource(arg, Resource::NoLock);
            pchFiles.append(resource.hashedFileName(Resource::AST));
            clangArgs[idx++] = pchFiles.last().constData();
        } else {
            clangArgs[idx++] = arg.constData();
            if (arg == "-include-pch") {
                nextIsPch = true;
            }
        }
        clangLine += arg;
        clangLine += " ";
    }
    clangLine += m_in;
    log(1) << "loading unit" << clangLine;

    CXIndex index = clang_createIndex(1, 1);
    CXTranslationUnit unit = clang_parseTranslationUnit(index, m_in.constData(),
                                                        clangArgs.data(), idx,
                                                        0, 0, CXTranslationUnit_Incomplete);
    if (unit) {
        debug() << "visiting" << m_in;
        clang_getInclusions(unit, inclusionVisitor, this);
        clang_visitChildren(clang_getTranslationUnitCursor(unit), indexVisitor, this);
        for (QHash<RTags::Location, IndexerJob::CursorInfo>::const_iterator it = m_cursorInfo.begin(); it != m_cursorInfo.end(); ++it) {
            debug() << it.key().key() << it.value().symbolLength << "=>" << it.value().target.key();
        }

        addFileNameSymbol(m_path);

        // m_impl->syncer->addSet(Database::Definition, m_defs);
        // m_defs.clear();

        // m_impl->syncer->addSet(Database::Reference, m_refs);
        // m_refs.clear();

        clang_disposeTranslationUnit(unit);
    } else {
        error() << "got 0 unit for" << m_in;
    }
    clang_disposeIndex(index);

    emit done(m_id, m_in);
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

int Indexer::index(const QByteArray& input, const QList<QByteArray>& arguments)
{
    QMutexLocker locker(&m_impl->implMutex);

    if (m_impl->indexing.contains(input))
        return -1;

    int id;
    do {
        id = m_impl->lastJobId++;
    } while (m_impl->jobs.contains(id));

    m_impl->indexing.insert(input);

    IndexerJob* job = new IndexerJob(m_impl, id, m_impl->path, input, arguments);
    m_impl->jobs[id] = job;
    connect(job, SIGNAL(done(int, QByteArray)), this, SLOT(jobDone(int, QByteArray)), Qt::QueuedConnection);

    if (!m_impl->timerRunning) {
        m_impl->timerRunning = true;
        m_impl->timer.restart();
    }

    QThreadPool::globalInstance()->start(job);

    return id;
}

void Indexer::jobDone(int id, const QByteArray& input)
{
    Q_UNUSED(input)

    QMutexLocker locker(&m_impl->implMutex);
    m_impl->jobs.remove(id);
    if (m_impl->indexing.remove(input))
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
