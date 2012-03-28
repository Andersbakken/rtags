#include "Indexer.h"
#include "RTags.h"
#include "Rdm.h"
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
#include "SHA256.h"

// #define RDM_TIMING
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
    QMutexLocker lock(&mMutex);
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
    QMutexLocker lock(&mMutex);
    if (mSymbols.isEmpty()) {
        mSymbols = symbols;
    } else {
        const SymbolHash::const_iterator end = symbols.end();
        for (SymbolHash::const_iterator it = symbols.begin(); it != end; ++it) {
            mSymbols[it.key()].unite(it.value());
        }
    }
}

// template <typename ### the merging should be templatized

void IndexerSyncer::run()
{
    while (true) {
        SymbolNameHash symbolNames;
        SymbolHash symbols;
        {
            QMutexLocker locker(&mMutex);
            if (mStopped)
                return;
            while (mSymbols.isEmpty() && mSymbolNames.isEmpty()) {
                mCond.wait(&mMutex, 10000);
                if (mStopped)
                    return;

            }
            qSwap(symbolNames, mSymbolNames);
            qSwap(symbols, mSymbols);
        }
        if (!symbolNames.isEmpty()) {
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

            SymbolNameHash::iterator it = symbolNames.begin();
            const SymbolNameHash::const_iterator end = symbolNames.end();
            bool changed = false;
            while (it != end) {
                const char *key = it.key().constData();
                const QSet<RTags::Location> added = it.value();
                QSet<RTags::Location> current = Rdm::readValue<QSet<RTags::Location> >(db, key);
                const int oldSize = current.size();
                current += added;
                if (current.size() != oldSize) {
                    changed = true;
                    Rdm::writeValue<QSet<RTags::Location> >(&batch, key, current);
                }
                ++it;
            }

            if (changed)
                db->Write(leveldb::WriteOptions(), &batch);
            delete db;
        }
        if (!symbols.isEmpty()) {
            leveldb::DB* db = 0;
            leveldb::Options options;
            options.create_if_missing = true;
            const QByteArray name = Database::databaseName(Database::Symbol);
            if (name.isEmpty())
                return;

            leveldb::Status status = leveldb::DB::Open(options, name.constData(), &db);
            if (!status.ok())
                return;
            Q_ASSERT(db);

            leveldb::WriteBatch batch;
            const leveldb::ReadOptions readopts;

            SymbolHash::iterator it = symbols.begin();
            const SymbolHash::const_iterator end = symbols.end();
            bool changed = false;
            while (it != end) {
                const QByteArray key = it.key().key(RTags::Location::Padded);
                Rdm::CursorInfo added = it.value();
                Rdm::CursorInfo current = Rdm::readValue<Rdm::CursorInfo>(db, key.constData());
                if (current.unite(added)) {
                    changed = true;
                    Rdm::writeValue<Rdm::CursorInfo>(&batch, key, current);
                }
                ++it;
            }

            if (changed)
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
    QSet<QByteArray> pchHeaderError;

    QByteArray path;
    int lastJobId;
    QHash<int, IndexerJob*> jobs;

    IndexerSyncer* syncer;

    bool timerRunning;
    QElapsedTimer timer;

    QMutex incMutex;
    QList<QByteArray> defaultArgs;
};

struct Timestamp
{
    Timestamp()
        : count(0), ms(0)
    {}
    inline void add(int t)
    {
        ms += t;
        ++count;
    }
    int count, ms;
};

class IndexerJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    IndexerJob(IndexerImpl* impl, int id,
               const QByteArray& path, const QByteArray& input,
               const QList<QByteArray>& arguments);

    int id() const { return mId; }

    void run();

    int mId;
    RTags::Location createLocation(CXCursor cursor);
    void addNamePermutations(CXCursor cursor, const RTags::Location &location);

    SymbolHash mSymbols;
    SymbolNameHash mSymbolNames;

    QSet<Path> mPaths;
    QHash<RTags::Location, QPair<RTags::Location, bool> > mReferences;
    QByteArray mPath, mIn;
    QList<QByteArray> mArgs;
    IndexerImpl* mImpl;
#ifdef RDM_TIMING
    QHash<int, Timestamp> mTimeStamps;
#endif
signals:
    void done(int id, const QByteArray& input);
};

#include "Indexer.moc"

// static inline void addInclusion(IndexerJob* job, CXFile inc)
// {
//     CXString str = clang_getFileName(inc);

//     const QByteArray path = Path::resolved(clang_getCString(str));

//     QMutexLocker locker(&job->mImpl->incMutex);
//     if (!qstrcmp(job->mIn, path)) {
//         clang_disposeString(str);
//         return;
//     }

//     job->mImpl->incs[path].insert(job->mIn);
//     clang_disposeString(str);
// }

static void inclusionVisitor(CXFile included_file,
                             CXSourceLocation*,
                             unsigned include_len,
                             CXClientData client_data)
{
    (void)client_data;
    (void)include_len;
    (void)included_file;
    IndexerJob* job = static_cast<IndexerJob*>(client_data);
    CXString fn = clang_getFileName(included_file);
    const char *cstr = clang_getCString(fn);
    if (strncmp("/usr/", cstr, 5) != 0) {
        printf("%s %d\n", Rdm::eatString(clang_getFileName(included_file)).constData(), include_len);
    }
    clang_disposeString(fn);
}


void IndexerJob::addNamePermutations(CXCursor cursor, const RTags::Location &location)
{
    QByteArray qname;
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
        Q_ASSERT(!qparam.isEmpty());
        mSymbolNames[qparam].insert(location);
        if (qparam != qnoparam) {
            Q_ASSERT(!qnoparam.isEmpty());
            mSymbolNames[qnoparam].insert(location);
        }

        clang_disposeString(displayName);
        cur = clang_getCursorSemanticParent(cur);
    }
}

RTags::Location IndexerJob::createLocation(CXCursor cursor)
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
            mPaths.insert(ret.path);
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

#ifdef RDM_TIMING
#define RDM_TIMESTAMP() job->mTimeStamps[__LINE__].add(timer.restart())
#define RDM_END_TIMESTAMP(file)                                         \
    printf("%s\n", file);                                               \
    for (QHash<int, Timestamp>::const_iterator it = mTimeStamps.begin(); it != mTimeStamps.end(); ++it) { \
        printf("    line: %d total: %dms count: %d average: %fms\n",    \
               it.key(), it.value().ms, it.value().count,               \
               it.value().count                                         \
               ? double(it.value().ms) / it.value().count               \
               : 0.0);                                                  \
    }                                                                   \

#else
#define RDM_TIMESTAMP()
#define RDM_END_TIMESTAMP(file)
#endif

static CXChildVisitResult indexVisitor(CXCursor cursor,
                                       CXCursor /*parent*/,
                                       CXClientData client_data)
{
#ifdef QT_DEBUG
    {
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (clang_equalCursors(cursor, ref) && !clang_isCursorDefinition(ref)) {
            ref = clang_getCursorDefinition(ref);
        }
        debug() << Rdm::cursorToString(cursor) << "refs" << Rdm::cursorToString(clang_getCursorReferenced(cursor))
                << (clang_equalCursors(ref, clang_getCursorReferenced(cursor)) ? QByteArray() : ("changed to " + Rdm::cursorToString(ref)));
    }
#endif
#ifdef RDM_TIMING
    QElapsedTimer timer;
    timer.start();
#endif
    IndexerJob* job = static_cast<IndexerJob*>(client_data);

    const CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_CXXAccessSpecifier:
        return CXChildVisit_Recurse;
    default:
        break;
    }

    const RTags::Location loc = job->createLocation(cursor);
    RDM_TIMESTAMP();
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
    RDM_TIMESTAMP();

    Rdm::CursorInfo &info = job->mSymbols[loc];
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
        RDM_TIMESTAMP();
    }

    if (clang_isCursorDefinition(cursor) || kind == CXCursor_FunctionDecl) {
        job->addNamePermutations(cursor, loc);
        RDM_TIMESTAMP();
    }


    if (!clang_isInvalid(refKind) && !clang_equalCursors(cursor, ref)) {
        const RTags::Location refLoc = job->createLocation(ref);
        RDM_TIMESTAMP();
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
                // error() << "got shit called" << loc << "ref is" << refLoc
                //         << Rdm::cursorToString(cursor) << "is" << Rdm::cursorToString(ref);
                break;
            default:
                break;
            }
        }
        job->mReferences[loc] = qMakePair(refLoc, isMemberFunction);
        RDM_TIMESTAMP();
    }
    return CXChildVisit_Recurse;

}

IndexerJob::IndexerJob(IndexerImpl* impl, int id,
                       const QByteArray& path, const QByteArray& input,
                       const QList<QByteArray>& arguments)
    : mId(id), mPath(path), mIn(input), mArgs(arguments), mImpl(impl)
{
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
        } else if (arg == "-include-pch") {
            nextIsPch = true;
        }
    }
    return out;
}

static QByteArray pchFileName(const QByteArray &path, const QByteArray &header)
{
    return path + SHA256::hash(header.constData());
}

void IndexerJob::run()
{
    QElapsedTimer timer;
    timer.start();
    QList<QByteArray> args = mArgs;
    QList<QByteArray> pchFiles = extractPchFiles(args);
    if (!pchFiles.isEmpty()) {
        QMutexLocker locker(&mImpl->implMutex);
        bool wait;
        do {
            wait = false;
            foreach (const QByteArray &pchFile, pchFiles) {
                if (mImpl->pchHeaderError.contains(pchFile)) {
                    int idx = args.indexOf(pchFile);
                    Q_ASSERT(idx > 0);
                    args.removeAt(idx);
                    args.removeAt(idx - 1);
                } else if (mImpl->indexing.contains(pchFile)) {
                    wait = true;
                    break;
                }
            }
            if (wait) {
                mImpl->implCond.wait(&mImpl->implMutex);
            }
        } while (wait);
    }
    const quint64 waitingForPch = timer.restart();

    QVarLengthArray<const char*, 32> clangArgs(args.size());
    QByteArray clangLine = "clang ";
    bool nextIsPch = false, nextIsX = false;
    QByteArray pchName;
    bool isPch = false;

    int idx = 0;
    foreach(const QByteArray& arg, args) {
        if (arg.isEmpty())
            continue;

        if (nextIsPch) {
            nextIsPch = false;
            pchFiles.append(pchFileName(mImpl->path, arg));
            clangArgs[idx++] = pchFiles.last().constData();
            clangLine += pchFiles.last().constData();
            clangLine += " ";
            continue;
        }

        if (nextIsX) {
            nextIsX = false;
            isPch = (arg == "c++-header" || arg == "c-header");
        }
        clangArgs[idx++] = arg.constData();
        clangLine += arg;
        clangLine += " ";
        if (arg == "-include-pch") {
            nextIsPch = true;
        } else if (arg == "-x") {
            nextIsX = true;
        }
    }
    if (isPch) {
        pchName = pchFileName(mImpl->path, mIn);
    }
    clangLine += mIn;

    CXIndex index = clang_createIndex(1, 1);
    CXTranslationUnit unit = clang_parseTranslationUnit(index, mIn.constData(),
                                                        clangArgs.data(), idx,
                                                        0, 0, CXTranslationUnit_Incomplete);
    log(1) << "loading unit" << clangLine << (unit != 0);
    bool pchError = false;

    if (!unit) {
        pchError = isPch;
        error() << "got 0 unit for" << clangLine;
    } else {
        clang_getInclusions(unit, inclusionVisitor, this);
        clang_visitChildren(clang_getTranslationUnitCursor(unit), indexVisitor, this);
        RDM_END_TIMESTAMP(mIn.constData());
        if (isPch) {
            Q_ASSERT(!pchName.isEmpty());
            if (clang_saveTranslationUnit(unit, pchName.constData(), clang_defaultSaveOptions(unit)) != CXSaveError_None) {
                error() << "Couldn't save pch file" << mIn << pchName;
                pchError = true;
            }
        }
        clang_disposeTranslationUnit(unit);

        const QHash<RTags::Location, QPair<RTags::Location, bool> >::const_iterator end = mReferences.end();
        for (QHash<RTags::Location, QPair<RTags::Location, bool> >::const_iterator it = mReferences.begin(); it != end; ++it) {
            SymbolHash::iterator sym = mSymbols.find(it.value().first);
            if (sym != mSymbols.end()) {
                // Q_ASSERT(mSymbols.contains(it.value().first));
                // debug() << "key" << it.key() << "value" << it.value();
                Rdm::CursorInfo &ci = sym.value();
                if (it.value().second) {
                    Rdm::CursorInfo &otherCi = mSymbols[it.key()];
                    // ### kinda nasty
                    ci.references += otherCi.references;
                    otherCi.references = ci.references;
                    if (otherCi.target.isNull())
                        ci.target = it.key();
                } else {
                    ci.references.insert(it.key());
                }
            }
        }

        {
            SymbolHash::iterator it = mSymbols.begin();
            const SymbolHash::const_iterator end = mSymbols.end();
            while (it != end) {
                Rdm::CursorInfo &ci = it.value();
                if (ci.target.isNull() && ci.references.isEmpty()) {
                    it = mSymbols.erase(it);
                } else {
                    debug() << it.key() << it.value().symbolLength << "=>" << it.value().target
                            << it.value().references;
                    ++it;
                }
            }
        }
        foreach(const Path &path, mPaths) {
            const RTags::Location loc(path, 1);
            mSymbolNames[path].insert(loc);
            mSymbolNames[path.fileName()].insert(loc);
        }
        mImpl->syncer->addSymbols(mSymbols);
        mImpl->syncer->addSymbolNames(mSymbolNames);
    }
    clang_disposeIndex(index);
    if (isPch) {
        QMutexLocker locker(&mImpl->implMutex);
        if (pchError) {
            mImpl->pchHeaderError.insert(mIn);
        } else {
            mImpl->pchHeaderError.remove(mIn);
        }
    }
    emit done(mId, mIn);
    log(0) << "visited" << mIn << timer.elapsed() << "ms. Waited for pch" << waitingForPch;
}

Indexer* Indexer::sInst = 0;

Indexer::Indexer(const QByteArray& path, QObject* parent)
    : QObject(parent), mImpl(new IndexerImpl)
{
    Q_ASSERT(path.startsWith('/'));
    if (!path.startsWith('/'))
        return;
    QDir dir;
    dir.mkpath(path);

    mImpl->jobCounter = 0;
    mImpl->lastJobId = 0;
    mImpl->path = path;
    if (!mImpl->path.endsWith('/'))
        mImpl->path += '/';
    mImpl->timerRunning = false;
    mImpl->syncer = new IndexerSyncer(this);
    mImpl->syncer->start();

    sInst = this;
}

Indexer::~Indexer()
{
    sInst = 0;
    mImpl->syncer->stop();
    mImpl->syncer->wait();

    delete mImpl;
}

Indexer* Indexer::instance()
{
    return sInst;
}

int Indexer::index(const QByteArray& input, const QList<QByteArray>& arguments)
{
    QMutexLocker locker(&mImpl->implMutex);

    if (mImpl->indexing.contains(input))
        return -1;

    int id;
    do {
        id = mImpl->lastJobId++;
    } while (mImpl->jobs.contains(id));

    mImpl->indexing.insert(input);

    IndexerJob* job = new IndexerJob(mImpl, id, mImpl->path, input, arguments);
    mImpl->jobs[id] = job;
    connect(job, SIGNAL(done(int, QByteArray)), this, SLOT(jobDone(int, QByteArray)), Qt::QueuedConnection);

    if (!mImpl->timerRunning) {
        mImpl->timerRunning = true;
        mImpl->timer.start();
    }

    QThreadPool::globalInstance()->start(job);

    return id;
}

void Indexer::jobDone(int id, const QByteArray& input)
{
    Q_UNUSED(input)

        QMutexLocker locker(&mImpl->implMutex);
    mImpl->jobs.remove(id);
    if (mImpl->indexing.remove(input))
        mImpl->implCond.wakeAll();

    ++mImpl->jobCounter;

    if (mImpl->jobs.isEmpty() || mImpl->jobCounter == SYNCINTERVAL) {
        // {
        //         QMutexLocker inclocker(&mImpl->incMutex);
        //         // mImpl->syncer->addSet(Database::Include, mImpl->incs);
        //         mImpl->incs.clear();
        //     }
        //     mImpl->jobCounter = 0;

        if (mImpl->jobs.isEmpty()) {
            mImpl->syncer->notify();

            Q_ASSERT(mImpl->timerRunning);
            mImpl->timerRunning = false;
            log(0) << "jobs took" << mImpl->timer.elapsed() << "ms";
        }
    }

    emit indexingDone(id);
}

void Indexer::force()
{
    mImpl->syncer->notify();
    // ### need to wait for syncer to write
}

void Indexer::setDefaultArgs(const QList<QByteArray> &args)
{
    mImpl->defaultArgs = args;
}
