#include "UnitCache.h"
#include "Resource.h"
#include "Indexer.h"
#include "Path.h"
#include "RTags.h"
#include <QMutexLocker>
#include <QThread>
#include <QVector>
#include <QMap>
#include <QCoreApplication>
#include <QDebug>
#include <QVariant>
#include <QStringList>
#include <QThreadPool>
#include <QRunnable>
#include <QMetaEnum>

class MaybeRecompileJob : public QRunnable
{
public:
    MaybeRecompileJob(const Path& fn)
        : fileName(fn)
    {
    }
protected:
    void run()
    {
        log(1) << "acquiring" << fileName << "from memory";
        CachedUnit* unit = new CachedUnit(fileName, UnitCache::Memory);
        if (unit->unit()) {
            log(1) << "got unit for" << fileName << "recompiling...";
            int mode = UnitCache::Source | UnitCache::Info;
            delete unit;
            unit = new CachedUnit(fileName, mode);
            if (unit->unit()) {
                Indexer::instance()->index(fileName, QList<QByteArray>(),
                                           UnitCache::AST|UnitCache::Memory|UnitCache::Info|UnitCache::ForceReindex);
            }
            log(1) << "recompiled" << fileName;
        } else {
            log(1) << "no unit for" << fileName;
        }
        delete unit;
    }

private:
    const Path fileName;
};

class MutexUnlocker
{
public:
    MutexUnlocker(QMutex* mutex)
        : m_mutex(mutex)
    {
        m_mutex->unlock();
    }
    ~MutexUnlocker()
    {
        m_mutex->lock();
    }
private:
    QMutex* m_mutex;
};

UnitCache* UnitCache::s_inst = 0;

static const int MaxUnits = 5;

static inline void printDiagnostic(CXTranslationUnit unit, const char* msg)
{
    unsigned int diagCount = clang_getNumDiagnostics(unit);
    for (unsigned int i = 0; i < diagCount; ++i) {
        const CXDiagnostic diag = clang_getDiagnostic(unit, i);
        const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diag);
        if (severity >= CXDiagnostic_Warning) {
            CXString dmsg = clang_formatDiagnostic(diag, CXDiagnostic_DisplaySourceLocation
                                                   | CXDiagnostic_DisplayColumn
                                                   | CXDiagnostic_DisplayOption
                                                   | CXDiagnostic_DisplayCategoryName);
            qWarning("%s: %s", msg, clang_getCString(dmsg));
            clang_disposeString(dmsg);
        }
        clang_disposeDiagnostic(diag);
    }
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

static inline QList<QByteArray> hashedPch(const QList<QByteArray>& args, QList<QByteArray>* pchs = 0)
{
    QList<QByteArray> out;
    bool nextIsPch = false;
    Resource resource;
    foreach(const QByteArray& arg, args) {
        if (arg.isEmpty())
            continue;

        if (nextIsPch) {
            nextIsPch = false;
            resource.setFileName(arg, Resource::NoLock);
            out.append(resource.hashedFileName(Resource::AST));
            if (pchs) {
                pchs->append(arg);
            }
            continue;
        }
        if (arg == "-include-pch")
            nextIsPch = true;
        out.append(arg);
    }
    return out;
}

UnitCache::UnitCache()
{
}

UnitCache::~UnitCache()
{
    foreach(UnitData* data, m_data) {
        data->ref = 0;
        data->owner = 0;
        destroyUnit(data);
    }
}

UnitCache* UnitCache::instance()
{
    if (!s_inst)
        s_inst = new UnitCache;
    return s_inst;
}

bool UnitCache::removeUnusedUnits(int num)
{
    QMap<QDateTime, QList<QByteArray> > candidates;

    QHash<QByteArray, UnitData*>::iterator it = m_data.begin();
    while (it != m_data.end()) {
        if (!it.value()->owner) {
            Q_ASSERT(!it.value()->ref);
            candidates[it.value()->unit.visited].append(it.key());
        }
        ++it;
    }

    // ### should improve the following loop, not extremely efficient
    bool done = false;
    int i = 0;
    QMap<QDateTime, QList<QByteArray> >::const_iterator cit = candidates.begin();
    const QMap<QDateTime, QList<QByteArray> >::const_iterator cend = candidates.end();
    forever {
        if (done || cit == cend)
            break;
        foreach(const QByteArray& entry, cit.value()) {
            if (i >= num) {
                done = true;
                break;
            }
            destroyUnit(m_data.value(entry));
            m_data.remove(entry);
            ++i;
        }
        ++cit;
    }

    Q_ASSERT(i <= num);

    return (i >= num);
}

inline bool UnitCache::rereadUnit(const QByteArray &sourceFileName,
                                  const QByteArray& hashedFilename,
                                  UnitData* data)
{
    data->unit.clear();
    data->unit.unit = clang_createTranslationUnit(data->unit.index,
                                                  hashedFilename.constData());
    if (data->unit.unit) {
        data->unit.origin = AST;
        data->unit.fileName = sourceFileName;
        data->unit.visited = QDateTime::currentDateTime();
        {
            QMutexLocker locker(&m_dataMutex);
            initFileSystemWatcher(&data->unit);
        }
        return true;
    } else {
        warning("failed to read unit from AST: %s (as %s)",
                data->unit.fileName.constData(), hashedFilename.constData());
    }
    return false;
}

inline bool UnitCache::loadUnit(const QByteArray& filename,
                                const QList<QByteArray>& arguments,
                                UnitData* data,
                                bool initWatcher,
                                bool *errors)
{
    if (errors)
        *errors = false;
    QVector<const char*> clangArgs;
    QList<QByteArray> args = hashedPch(arguments, &data->unit.pchs);
    QByteArray clangLine = "clang ";
    foreach(const QByteArray& arg, args) {
        clangArgs.append(arg.constData());
        clangLine += arg;
        clangLine += " ";
    }
    clangLine += filename;
    log(1) << "loading unit" << clangLine;

    data->unit.clear();

    data->unit.unit = clang_parseTranslationUnit(data->unit.index, filename.constData(),
                                                 clangArgs.data(), clangArgs.size(),
                                                 0, 0, CXTranslationUnit_Incomplete);
    if (data->unit.unit) {
        data->unit.origin = Source;
        data->unit.fileName = filename;
        data->unit.visited = QDateTime::currentDateTime();
        if (initWatcher) {
            QMutexLocker locker(&m_dataMutex);
            initFileSystemWatcher(&data->unit);
        }
        if (errors) {
            unsigned int diagCount = clang_getNumDiagnostics(data->unit.unit);
            for (unsigned int i = 0; i < diagCount; ++i) {
                int level = 0;
                const CXDiagnostic diag = clang_getDiagnostic(data->unit.unit, i);
                const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diag);
                switch (severity) {
                case CXDiagnostic_Ignored:
                case CXDiagnostic_Note:
                    level = 2;
                    break;
                case CXDiagnostic_Error:
                case CXDiagnostic_Fatal:
                    level = 0;
                    *errors = true;
                    break;
                case CXDiagnostic_Warning:
                    level = 1;
                    break;
                }
                if (!testLog(level))
                    continue;
                CXString msg = clang_formatDiagnostic(diag, CXDiagnostic_DisplaySourceLocation
                                                      | CXDiagnostic_DisplayColumn
                                                      | CXDiagnostic_DisplayOption
                                                      | CXDiagnostic_DisplayCategoryName);
                CXSourceLocation loc =  clang_getDiagnosticLocation(diag);
                CXFile f;
                unsigned l, c;
                clang_getSpellingLocation(loc, &f, &l, &c, 0);
                log(level, "clang: %s (%s)", clang_getCString(msg), data->unit.fileName.constData());

                clang_disposeString(msg);
                clang_disposeDiagnostic(diag);
            }
            if (*errors) {
                warning("got errors from source: %s", filename.constData());
                QByteArray clang = "clang ";
                for (int i=0; i<clangArgs.size(); ++i) {
                    clang += clangArgs.at(i);
                    clang += ' ';
                }
                warning("%s%s\n", clang.constData(), filename.constData());
            }
        }
        return true;
    } else {
        warning("failed to read unit from source: %s", filename.constData());
        QByteArray clang = "clang ";
        for (int i=0; i<clangArgs.size(); ++i) {
            clang += clangArgs.at(i);
            clang += ' ';
        }
        warning("%s%s\n", clang.constData(), filename.constData());
    }
    return false;
}

inline bool UnitCache::saveUnit(UnitData* data,
                                Resource* resource,
                                const QList<QByteArray>& arguments,
                                unsigned flags)
{
    bool ok = true;
    const QByteArray hashedFilename = resource->hashedFileName(Resource::AST);
    if (flags & SaveAST) {
        const int result = clang_saveTranslationUnit(data->unit.unit,
                                                     hashedFilename.constData(),
                                                     CXSaveTranslationUnit_None);
        if (result != CXSaveError_None) {
            qWarning("Unable to save translation unit: %s (as %s)",
                     data->unit.fileName.constData(), hashedFilename.constData());
            printDiagnostic(data->unit.unit, "save (1)");
            ok = false;
        } else {
            log(1) << "successfully saved" << hashedFilename;
        }
    }
    if (flags & SaveInfo) {
        resource->write(Resource::Information, (QList<QByteArray>() << "# " + data->unit.fileName) + arguments,
                        Resource::Truncate);
        log(1) << "wrote inf file for" << data->unit.fileName << arguments;
    }
    return ok;
}

inline void UnitCache::destroyUnit(UnitData* data)
{
    data->unit.clear();
    clang_disposeIndex(data->unit.index);
}

static inline void removeComments(QList<QByteArray> &lines)
{
    QList<QByteArray> old = lines;
    for (int i=lines.size() - 1; i>=0; --i) {
        if (lines.at(i).startsWith("#"))
            lines.removeAt(i);
    }
}

inline bool UnitCache::recheckPch(const QList<QByteArray>& arguments, UnitData* data)
{
    bool reread = false;
    const QList<QByteArray> pchFiles = extractPchFiles(arguments);
    foreach(const QByteArray& pchFile, pchFiles) {
        Q_ASSERT(!data->unit.unit);
        Resource resource(pchFile);
        QList<QByteArray> pchArgs = resource.read<QList<QByteArray> >(Resource::Information);
        removeComments(pchArgs);
        bool errors;
        if (loadUnit(pchFile, pchArgs, data, false, &errors)) {
            if (saveUnit(data, &resource, pchArgs, errors ? SaveInfo : SaveInfo|SaveAST))
                reread = true;
        }
        data->unit.clear();
    }
    return reread;
}

inline UnitCache::UnitStatus UnitCache::initUnit(const QByteArray& input,
                                                 const QByteArray& output,
                                                 const QList<QByteArray>& args,
                                                 int mode, UnitData* data)
{
    if (mode & (Source|AST)) {
        QList<QByteArray> arguments = args;

        // we don't mind rereading from disk
        if (!data->owner || (data->owner == QThread::currentThread() && !data->unit.unit)) {
            // and luckily we can
            if (!data->owner) {
                data->owner = QThread::currentThread();
                Q_ASSERT(!data->ref);
                ++data->ref;
            }

            // and we don't need to hold the mutex while doing so
            MutexUnlocker unlocker(&m_dataMutex);

            bool pchRechecked = false;
            Resource resource(output);
            if (mode & AST) { // try to reread AST
                if (resource.exists(Resource::AST)) {
                    bool retry = false;
                    do {
                        if (rereadUnit(output, resource.hashedFileName(Resource::AST), data)) {
                            // done!
                            return Done;
                        } else {
                            if (retry)
                                break;
                            // recheck pch;
                            retry = recheckPch(arguments, data);
                            pchRechecked = true;
                        }
                    } while (retry);
                }
                // Unable to read AST, get the .inf arguments if we didn't specify a source read
                // or if the source read specified an info read
                if (((mode & Source) && (mode & Info)) || !(mode & Source)) {
                    if (resource.exists(Resource::Information)) {
                        arguments = resource.read<QList<QByteArray> >(Resource::Information);
                        removeComments(arguments);
                        mode |= Source;
                        mode &= ~Info;
                    } else {
                        return Abort;
                    }
                }
            }
            if (mode & Source) {
                // Read the .inf arguments if we asked for it
                if (mode & Info) {
                    if (resource.exists(Resource::Information)) {
                        arguments = resource.read<QList<QByteArray> >(Resource::Information);
                        removeComments(arguments);
                    } else {
                        return Abort;
                    }
                }
                bool errors;
                if (loadUnit(input, arguments, data, true, &errors)) {
                    saveUnit(data, &resource, arguments, errors ? SaveInfo : SaveInfo|SaveAST);
                    return Done;
                }
                bool retry = false;
                do {
                    if (loadUnit(input, arguments, data, true, &errors)) {
                        saveUnit(data, &resource, arguments, errors ? SaveInfo : SaveInfo|SaveAST);
                        // done!
                        return Done;
                    } else {
                        if (retry)
                            break;
                        // recheck pch files, if any
                        retry = (!pchRechecked && recheckPch(arguments, data));
                    }
                } while (retry);
                // Unable to read or write source, not good
                return Abort;
            }
        } else {
            // but we can't, so we have to wait
            return Wait;
        }
    }

    Q_ASSERT_X(false, "UnitCache::initUnit()", "Should not happen");
    return Abort;
}

UnitCache::Unit* UnitCache::createUnit(const QByteArray& input,
                                       const QByteArray& output,
                                       const QList<QByteArray>& args,
                                       int mode)
{
    if (!mode)
        return 0;

    QMutexLocker lock(&m_dataMutex);
    for (;;) {
        const QHash<QByteArray, UnitData*>::iterator it = m_data.find(input);
        if (it != m_data.end()) {
            // the unit exists in our cache
            if (mode & Memory) {
                // we want to use a unit from the cache
                if (it.value()->owner == QThread::currentThread() || !it.value()->owner) {
                    it.value()->owner = QThread::currentThread();
                    ++it.value()->ref;
                    it.value()->unit.visited = QDateTime::currentDateTime();
                    it.value()->unit.origin = Memory;
                    return &it.value()->unit;
                }
                if (mode == Memory) {
                    // and we really really want to use a unit in our cache
                    m_dataCondition.wait(&m_dataMutex);
                    continue; // recheck
                }
            }

            const UnitStatus status = initUnit(input, output, args, mode, it.value());

            if (status == Done) {
                it.value()->unit.visited = QDateTime::currentDateTime();
                return &it.value()->unit;
            } else if (status == Abort) {
                UnitData* data = it.value();
                destroyUnit(data);
                m_data.erase(it);
                delete data;
                m_dataCondition.wakeAll();
                return 0;
            } else if (status == Wait) {
                m_dataCondition.wait(&m_dataMutex);
                continue; // recheck
            }
            Q_ASSERT_X(false, "UnitCache::createUnit", "Should not happen");
        } else {
            // the unit doesn't exist in our cache
            if (mode == Memory) {
                // and we only wanted a unit if it was in our cache, so return 0
                return 0;
            }

            const int sz = m_data.size();
            if (sz >= MaxUnits && !removeUnusedUnits(sz - MaxUnits + 1)) {
                // no available slots in our cache, we need to wait
                m_dataCondition.wait(&m_dataMutex);
                continue; // recheck
            } else {
                // slots are available, create a new unit
                UnitData* data = new UnitData;

                // set up our new unit data
                data->ref = 1;
                data->owner = QThread::currentThread();
                data->unit.visited = QDateTime::currentDateTime();
                data->unit.fileName = input;
                m_data[input] = data;

                data->unit.index = clang_createIndex(1, 1);

                const UnitStatus status = initUnit(input, output, args, mode, data);

                if (status == Done) {
                    return &data->unit;
                } else if (status == Abort) {
                    destroyUnit(data);
                    m_data.remove(input);
                    delete data;
                    m_dataCondition.wakeAll();
                    return 0;
                } else if (status == Wait) {
                    m_dataCondition.wait(&m_dataMutex);
                    continue; // recheck
                }
                Q_ASSERT_X(false, "UnitCache::createUnit", "Should not happen 2");
            }
        }
    }
    return 0;
}

void UnitCache::release(Unit* unit)
{
    if (!unit)
        return;

    QMutexLocker locker(&m_dataMutex);
    const QHash<QByteArray, UnitData*>::iterator it = m_data.find(unit->fileName);
    Q_ASSERT(it != m_data.end());
    Q_ASSERT(it.value()->owner == QThread::currentThread());
    Q_ASSERT(it.value()->ref > 0);
    if (!--it.value()->ref) {
        it.value()->owner = 0;
        m_dataCondition.wakeAll();
    }
}

void CachedUnit::adopt(UnitCache::Unit* unit)
{
    if (unit == m_unit)
        return;

    UnitCache* cache = UnitCache::instance();
    cache->release(m_unit);
    if (unit)
        m_unit = cache->acquire(unit->fileName);
    else
        m_unit = 0;
}

struct FindIncludesUserData {
    QByteArray path;
    QHash<Path, QSet<QByteArray> > paths;
};
static void findIncludes(CXFile includedFile, CXSourceLocation* stack, unsigned stackSize, CXClientData userData)
{
    Q_UNUSED(stack);
    Q_UNUSED(stackSize);
    CXString fn = clang_getFileName(includedFile);
    const char *cstr = clang_getCString(fn);
    FindIncludesUserData *u = reinterpret_cast<FindIncludesUserData*>(userData);
    if (strncmp(cstr, "/usr/", 5)) {
        QHash<Path, QSet<QByteArray> > &paths = u->paths;
        Path p(cstr);
        p.canonicalizePath();
        paths[p.parentDir()].insert(p.fileName());
    }
    // log(1, "%s for %s", cstr, u->path.constData());
    // for (unsigned i=0; i<stackSize; ++i) {
    //     CXFile f;
    //     unsigned l, c;
    //     clang_getSpellingLocation(stack[i], &f, &l, &c, 0);
    //     log(1, "%s:%d:%d (%d) for %s", RTags::Rdm::eatString(clang_getFileName(f)).constData(), l, c, stackSize, u->path.constData());
    // }

    clang_disposeString(fn);
}

void UnitCache::initFileSystemWatcher(Unit* unit)
{
    FindIncludesUserData u = { unit->fileName, QHash<Path, QSet<QByteArray> >() };
    clang_getInclusions(unit->unit, findIncludes, &u);
    foreach(const QByteArray& pch, unit->pchs) {
        Path p(pch);
        QHash<Path, QSet<QByteArray> > pchDeps;
        pchDeps = m_cachedPchDependencies.value(p);
        debug() << "Getting dependencies for" << p << pchDeps.size() << unit->fileName;
        for (QHash<Path, QSet<QByteArray> >::const_iterator it = pchDeps.begin(); it != pchDeps.end(); ++it) {
            u.paths[it.key()] += it.value();
        }
        u.paths[p.parentDir()].insert(p.fileName());
    }
    Q_ASSERT(unit->fileName.isResolved());
    if (unit->fileName.isHeader() && (unit->origin == Source || !m_cachedPchDependencies.contains(unit->fileName))) {
        m_cachedPchDependencies[unit->fileName] = u.paths;
        debug() << "caching pch dependencies for" << unit->fileName << "to" << u.paths;
    }
    // log(1) << "got paths" << unit->fileName << u.paths << "pchs" << unit->pchs;
    // log(1) << paths.keys();
    FileSystemWatcher *old = m_watchers.take(unit->fileName);
    if (!u.paths.isEmpty()) {
        FileSystemWatcher* watcher = new FileSystemWatcher(unit->fileName);
        watcher->moveToThread(thread());
        m_watchers[unit->fileName] = watcher;
        QStringList dirs;
        for (QHash<Path, QSet<QByteArray> >::iterator it = u.paths.begin(); it != u.paths.end(); ++it) {
            dirs.append(it.key());
            // log(1) << "watching" << it.value() << "in" << it.key() << "for" << unit->fileName;
        }
        watcher->paths = u.paths;
        watcher->addPaths(dirs);
        connect(watcher, SIGNAL(directoryChanged(QString)),
                this, SLOT(onDirectoryChanged(QString)));
    }
    delete old;
}

void UnitCache::freeFileSystemWatcher(const QByteArray& filename) // always called with m_dataMutex held
{
    FileSystemWatcher* watcher = m_watchers.take(filename);
    Q_ASSERT(watcher);
    delete watcher;
}

void UnitCache::onDirectoryChanged(const QString &directory)
{
    const Path dir(directory.toLocal8Bit());
    QMutexLocker lock(&m_dataMutex);
    FileSystemWatcher *f = qobject_cast<FileSystemWatcher*>(sender());
    if (f) {
        bool dirty = false;
        const QSet<QByteArray> &fileNames = f->paths.value(dir);
        Q_ASSERT(!fileNames.isEmpty());
        const QByteArray dirName = dir + "/";
        foreach(const QByteArray &fn, fileNames) {
            const Path p(dirName + fn);
            if (p.lastModified() > f->lastModified) {
                log(1) << "recompiling" << f->fileName << "since lastModified for dir was"
                       << QDateTime::fromTime_t(f->lastModified)
                       << "and" << fn << "was modified at" << QDateTime::fromTime_t(p.lastModified());
                dirty = true;
                break;
            }
        }
        if (dirty) {
            QThreadPool::globalInstance()->start(new MaybeRecompileJob(f->fileName));
        }
    }
}

void UnitCache::Unit::clear()
{
    if (unit) {
        clang_disposeTranslationUnit(unit);
        unit = 0;
    }
    origin = None;
    fileName.clear();
    visited = QDateTime();
    pchs.clear();
}
QList<QByteArray> UnitCache::status()
{
    QList<QByteArray> ret;
    QMutexLocker lock(&m_dataMutex);
    QHash<QByteArray, UnitData*>::iterator it = m_data.begin();
    const QMetaEnum enumerator = metaObject()->enumerator(metaObject()->indexOfEnumerator("LoadFlag"));
    int i = 1;
    while (it != m_data.end()) {
        char buf[1024];
        const int written = snprintf(buf, 1024, "%d/%d: ref %d origin %s fileName %s visited %s pchs %s",
                                     i++, m_data.size(), it.value()->ref,
                                     enumerator.valueToKeys(it.value()->unit.origin).constData(),
                                     it.value()->unit.fileName.constData(),
                                     it.value()->unit.visited.toString().toLocal8Bit().constData(),
                                     RTags::join(it.value()->unit.pchs, "|").constData());
        ret.append(QByteArray(buf, written));
    }
    return ret;
}
