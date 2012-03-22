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

typedef QHash<RTags::Location, Rdm::CursorInfo> SymbolHash;
typedef QHash<QByteArray, QSet<RTags::Location> > SymbolNameHash;
class IndexerJob;
class IndexerSyncer : public QThread
{
public:
    IndexerSyncer(QObject* parent = 0);

    void addSymbols(const SymbolHash &data);
    void addSymbolNames(const SymbolNameHash &symbolNames);
    void notify();
    void stop();

protected:
    void run();

private:
    bool mStopped;
    QMutex mMutex;
    QWaitCondition mCond;
    SymbolHash mSymbols;
    SymbolNameHash mSymbolNames;
};

IndexerSyncer::IndexerSyncer(QObject* parent)
    : QThread(parent), mStopped(false)
{
}

void IndexerSyncer::stop()
{
    QMutexLocker locker(&mMutex);
    mStopped = true;
    mCond.wakeOne();
}

void IndexerSyncer::notify()
{
    QMutexLocker locker(&mMutex); // is this needed here?
    mCond.wakeOne();
}

void IndexerSyncer::addSymbolNames(const SymbolNameHash &locations)
{
    if (mSymbolNames.isEmpty()) {
        mSymbolNames = locations;
    } else {
        const SymbolNameHash::const_iterator end = locations.end();
        for (SymbolNameHash::const_iterator it = locations.begin(); it != end; ++it) {
            mSymbolNames[it.key()].unite(it.value());
        }
    }
}

void IndexerSyncer::addSymbols(const SymbolHash &symbols)
{
    if (mSymbols.isEmpty()) {
        mSymbols = symbols;
    } else {
        const SymbolHash::const_iterator end = symbols.end();
        for (SymbolHash::const_iterator it = symbols.begin(); it != end; ++it) {
            mSymbols[it.key()].unite(it.value());
        }
    }
}

void IndexerSyncer::run()
{
    QMutexLocker locker(&mMutex);
    while (!mStopped) {
        mCond.wait(&mMutex, 10000);
        if (!mSymbolNames.isEmpty()) {
            leveldb::DB* db = 0;
            leveldb::Options options;
            options.create_if_missing = true;
            const QByteArray name = Database::databaseName(Database::SymbolName);
            if (name.isEmpty())
                return;

            leveldb::Status status = leveldb::DB::Open(options, name.constData(), &db);
            if (!status.ok())
                return;
            Q_ASSERT(db);

            leveldb::WriteBatch batch;
            const leveldb::ReadOptions readopts;

            SymbolNameHash::iterator it = mSymbolNames.begin();
            const SymbolNameHash::const_iterator end = mSymbolNames.end();
            while (it != end) {
                const char *key = it.key().constData();
                const QSet<RTags::Location> added = it.value();
                QSet<RTags::Location> current = Rdm::readValue<QSet<RTags::Location> >(db, key);
                const int oldSize = current.size();
                current += added;
                if (current.size() != oldSize)
                    Rdm::writeValue<QSet<RTags::Location> >(&batch, key, current);
                ++it;
            }

            db->Write(leveldb::WriteOptions(), &batch);
            delete db;
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

    bool timerRunning;
    QElapsedTimer timer;

    QMutex incMutex;
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

    SymbolHash m_cursorInfo;
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

// static inline void addInclusion(IndexerJob* job, CXFile inc)
// {
//     CXString str = clang_getFileName(inc);

//     const QByteArray path = Path::resolved(clang_getCString(str));

//     QMutexLocker locker(&job->m_impl->incMutex);
//     if (!qstrcmp(job->m_in, path)) {
//         clang_disposeString(str);
//         return;
//     }

//     job->m_impl->incs[path].insert(job->m_in);
//     clang_disposeString(str);
// }

// static void inclusionVisitor(CXFile included_file,
//                              CXSourceLocation*,
//                              unsigned include_len,
//                              CXClientData client_data)
// {
//     IndexerJob* job = static_cast<IndexerJob*>(client_data);
//     if (include_len)
//         addInclusion(job, included_file);
// }

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
    {
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (clang_equalCursors(cursor, ref) && !clang_isCursorDefinition(ref)) {
            ref = clang_getCursorDefinition(ref);
        }
        debug() << Rdm::cursorToString(cursor) << "refs" << Rdm::cursorToString(clang_getCursorReferenced(cursor))
                << (clang_equalCursors(ref, clang_getCursorReferenced(cursor)) ? QByteArray() : ("changed to " + Rdm::cursorToString(ref)));
    }
    IndexerJob* job = static_cast<IndexerJob*>(client_data);

    const CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_CXXAccessSpecifier:
        return CXChildVisit_Recurse;
    default:
        break;
    }

    const RTags::Location loc = job->createLocation(cursor);
    if (loc.isNull()) {
        return CXChildVisit_Recurse;
    }
    CXCursor ref = clang_getCursorReferenced(cursor);
    if (clang_equalCursors(cursor, ref) && !clang_isCursorDefinition(ref)) {
        // QByteArray old = Rdm::cursorToString(ref);
        ref = clang_getCursorDefinition(ref);
        // error() << "changed ref from" << old << "to" << Rdm::cursorToString(ref);
    }
    const CXCursorKind refKind = clang_getCursorKind(ref);

    Rdm::CursorInfo &info = job->m_cursorInfo[loc];
    if (kind == CXCursor_CallExpr && refKind == CXCursor_CXXMethod) {
        return CXChildVisit_Recurse;
    } else if (!info.symbolLength) {
        info.kind = kind;
    } else if (info.kind == CXCursor_Constructor && kind == CXCursor_TypeRef) {
        return CXChildVisit_Recurse;
    }
    if (!info.symbolLength) {
        CXString name;
        if (clang_isReference(kind)) {
            name = clang_getCursorSpelling(ref);
        } else {
            name = clang_getCursorSpelling(cursor);
        }
        const char *cstr = clang_getCString(name);
        info.symbolLength = cstr ? strlen(cstr) : 0;
        clang_disposeString(name);
    }

    if (clang_isCursorDefinition(cursor) || kind == CXCursor_FunctionDecl) {
        job->addNamePermutations(cursor);
    }


    if (!clang_isInvalid(refKind) && !clang_equalCursors(cursor, ref)) {
        const RTags::Location refLoc = job->createLocation(ref);
        if (refLoc.isNull()) {
            return CXChildVisit_Recurse;
        }

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
                error() << "got shit called" << loc << "ref is" << refLoc
                        << Rdm::cursorToString(cursor) << "is" << Rdm::cursorToString(ref);
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
        // clang_getInclusions(unit, inclusionVisitor, this);
        clang_visitChildren(clang_getTranslationUnitCursor(unit), indexVisitor, this);
        error() << "visiting" << m_in << m_references.size() << m_cursorInfo.size();

        const QHash<RTags::Location, QPair<RTags::Location, bool> >::const_iterator end = m_references.end();
        for (QHash<RTags::Location, QPair<RTags::Location, bool> >::const_iterator it = m_references.begin(); it != end; ++it) {
            Q_ASSERT(m_cursorInfo.contains(it.value().first));
            debug() << "key" << it.key() << "value" << it.value();
            Rdm::CursorInfo &ci = m_cursorInfo[it.value().first];
            if (it.value().second) {
                Rdm::CursorInfo &otherCi = m_cursorInfo[it.key()];
                if (otherCi.target.isNull())
                    ci.target = it.key();
            } else {
                ci.references.insert(it.key());
            }
        }


        for (SymbolHash::iterator it = m_cursorInfo.begin(); it != m_cursorInfo.end(); ++it) {
            Rdm::CursorInfo &ci = it.value();
            if (ci.target.isNull() && ci.references.isEmpty())
                continue;
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

    // if (m_impl->jobs.isEmpty() || m_impl->jobCounter == SYNCINTERVAL) {
    //     {
    //         QMutexLocker inclocker(&m_impl->incMutex);
    //         // m_impl->syncer->addSet(Database::Include, m_impl->incs);
    //         m_impl->incs.clear();
    //     }
    //     m_impl->jobCounter = 0;

    //     if (m_impl->jobs.isEmpty()) {
    //         m_impl->syncer->notify();

    //         Q_ASSERT(m_impl->timerRunning);
    //         m_impl->timerRunning = false;
    //         log(1) << "jobs took" << m_impl->timer.elapsed() << "ms";
    //     }
    // }

    emit indexingDone(id);
}
