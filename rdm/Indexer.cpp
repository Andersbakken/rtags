#include "Indexer.h"
#include "RTags.h"
#include "UnitCache.h"
#include "Rdm.h"
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

struct CursorInfo {
    CursorInfo() : symbolLength(0) {}
    bool isNull() const { return symbolLength; }
    int symbolLength;
    RTags::Location target;
    QSet<RTags::Location> references;
};

class IndexerJob;

typedef QHash<QByteArray, QSet<QByteArray> > HashSet;

class IndexerSyncer : public QThread
{
public:
    IndexerSyncer(QObject* parent = 0);

    void addData(const QHash<RTags::Location, CursorInfo> &data);
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
    RTags::Location createLocation(CXCursor cursor) const;
    void addNamePermutations(CXCursor cursor);

    QHash<RTags::Location, CursorInfo> m_cursorInfo;
    QHash<RTags::Location, QPair<RTags::Location, bool> > m_references;
    QByteArray m_path, m_in;
    QList<QByteArray> m_args;
    IndexerImpl* m_impl;

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

RTags::Location IndexerJob::createLocation(CXCursor cursor) const
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    // if (clang_equalRanges(range, m_lastSourceRange)) {
    //     *len = m_lastLocationLength;
    //     return m_lastLocation;
    // }

    RTags::Location ret;
    if (!clang_equalLocations(location, clang_getNullLocation())) {
        CXFile file;
        unsigned start;
        clang_getSpellingLocation(location, &file, 0, 0, &start);
        CXString fn = clang_getFileName(file);
        const char *fileName = clang_getCString(fn);
        if (fileName && strlen(fileName)) {
            ret.path = fileName;
            ret.path.canonicalizePath(); // ### could canonicalize directly
            ret.offset = start;
        }
        // unsigned l, c;
        // clang_getSpellingLocation(location, 0, &l, &c, 0);
        // QByteArray out;
        // out.append(ret.path);
        // out.append(':');
        // out.append(QByteArray::number(l));
        // out.append(':');
        // out.append(QByteArray::number(c));
        // debug() << ret.key() << "is" << out;
        clang_disposeString(fn);
    }
    return ret;
}

static CXChildVisitResult indexVisitor(CXCursor cursor,
                                       CXCursor /*parent*/,
                                       CXClientData client_data)
{
    IndexerJob* job = static_cast<IndexerJob*>(client_data);
    
    const CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_CXXAccessSpecifier:
        return CXChildVisit_Recurse;
    default:
        break;
    }

    error() << Rdm::cursorToString(cursor) << "refs" << Rdm::cursorToString(clang_getCursorReferenced(cursor));
    const RTags::Location loc = job->createLocation(cursor);
    if (loc.isNull() || job->m_cursorInfo.contains(loc)) {
        return CXChildVisit_Recurse;
    }
    CursorInfo &info = job->m_cursorInfo[loc];
    info.symbolLength = 1;

    if (clang_isCursorDefinition(cursor) || kind == CXCursor_FunctionDecl) {
        job->addNamePermutations(cursor);
    }

    CXCursor ref = clang_getCursorReferenced(cursor);
    if (clang_equalCursors(cursor, ref) && !clang_isCursorDefinition(ref)) {
        QByteArray old = Rdm::cursorToString(ref);
        ref = clang_getCursorDefinition(ref);
        // error() << "changed ref from" << old << "to" << Rdm::cursorToString(ref);
    }

    CXCursorKind refKind = clang_getCursorKind(ref);

    if (!clang_isInvalid(refKind) && !clang_equalCursors(cursor, ref)) {
        const RTags::Location refLoc = job->createLocation(ref);
        if (refLoc.isNull())
            return CXChildVisit_Recurse;

        info.target = refLoc;
        bool isMemberFunction = false;
        // error() << "we're here" << Rdm::cursorToString(ref)
        //         << Rdm::cursorToString(cursor);
        if (refKind == kind) {
            switch (refKind) {
            case CXCursor_Constructor:
            case CXCursor_Destructor:
            case CXCursor_CXXMethod:
                isMemberFunction = true;
                // error() << "got shit called" << loc << "ref is" << refLoc
                //         << Rdm::cursorToString(cursor) << "is" << Rdm::cursorToString(ref);
                break;
            default:
                break;
            }
        }
        job->m_references[loc] = qMakePair(refLoc, isMemberFunction);
    }
    return CXChildVisit_Recurse;

}

IndexerJob::IndexerJob(IndexerImpl* impl, int id,
                       const QByteArray& path, const QByteArray& input,
                       const QList<QByteArray>& arguments)
    : m_id(id), m_path(path), m_in(input), m_args(arguments), m_impl(impl)
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

    CXIndex index = clang_createIndex(1, 1);
    CXTranslationUnit unit = clang_parseTranslationUnit(index, m_in.constData(),
                                                        clangArgs.data(), idx,
                                                        0, 0, CXTranslationUnit_Incomplete);
    log(1) << "loading unit" << clangLine << (unit != 0);

    if (unit) {
        clang_getInclusions(unit, inclusionVisitor, this);
        clang_visitChildren(clang_getTranslationUnitCursor(unit), indexVisitor, this);
        error() << "visiting" << m_in << m_references.size() << m_cursorInfo.size();

        const QHash<RTags::Location, QPair<RTags::Location, bool> >::const_iterator end = m_references.end();
        for (QHash<RTags::Location, QPair<RTags::Location, bool> >::const_iterator it = m_references.begin(); it != end; ++it) {
            Q_ASSERT(m_cursorInfo.contains(it.value().first));
            CursorInfo &ci = m_cursorInfo[it.value().first];
            ci.references.insert(it.key());
            // if (it.value().second) {
            //     error() << "We got second" << ci.target.isNull() << it.key() << "for" << it.value().first;
            // }
            if (it.value().second && ci.target.isNull()) {
                CursorInfo &otherCi = m_cursorInfo[it.key()];
                QSet<RTags::Location> refs = ci.references + otherCi.references;
                // refs.insert(it.key());
                // refs.insert(it.value().first);
                ci.references = refs;
                otherCi.references = refs;
                ci.target = it.key();
            }
        }


        for (QHash<RTags::Location, CursorInfo>::iterator it = m_cursorInfo.begin(); it != m_cursorInfo.end(); ++it) {
            CursorInfo &ci = it.value();
            if (ci.target.isNull() && ci.references.isEmpty())
                continue;
            ci.references.insert(it.key());
            debug() << it.key() << it.value().symbolLength << "=>" << it.value().target
                    << it.value().references;
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
