#include "UnitCache.h"
#include "Resource.h"
#include "Indexer.h"
#include "Path.h"
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
        qDebug() << "acquiring" << fileName << "from memory";
        CachedUnit unit(fileName, UnitCache::Memory);
        if (unit.unit()) {
            qDebug() << "got unit for" << fileName << "recompiling...";
            UnitCache::instance()->recompile(unit.unit());
            qDebug() << "recompiled" << fileName;
        } else {
            qDebug() << "no unit for" << fileName;
        }
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

UnitCache::UnitCache()
{
}

UnitCache::~UnitCache()
{
    foreach(UnitData* data, m_data) {
        data->ref = 0;
        data->owner = 0;
        cleanup(data);
    }
}

UnitCache* UnitCache::instance()
{
    if (!s_inst)
        s_inst = new UnitCache;
    return s_inst;
}

inline void UnitCache::cleanup(UnitData* data)
{
    Q_ASSERT(data);
    Q_ASSERT(!data->owner);
    Q_ASSERT(!data->ref);
    clang_disposeTranslationUnit(data->unit.unit);
    clang_disposeIndex(data->unit.index);
    delete data;
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
            cleanup(m_data.value(entry));
            m_data.remove(entry);
            ++i;
        }
        ++cit;
    }

    Q_ASSERT(i <= num);

    return (i >= num);
}

inline bool UnitCache::rereadUnit(const QByteArray& hashedFilename,
                                  UnitData* data)
{
    data->unit.unit = clang_createTranslationUnit(data->unit.index,
                                                  hashedFilename.constData());
    if (data->unit.unit) {
        data->unit.file = clang_getFile(data->unit.unit, data->unit.fileName.constData());
        data->unit.origin = AST;
        initFileSystemWatcher(&data->unit);
        return true;
    } else {
        qDebug("failed to read unit from AST: %s (as %s)",
               data->unit.fileName.constData(), hashedFilename.constData());
    }
    return false;
}

inline bool UnitCache::loadUnit(const QByteArray& filename,
                                const QList<QByteArray>& arguments,
                                UnitData* data)
{
    QVector<const char*> clangArgs;
    foreach(const QByteArray& arg, arguments) {
        clangArgs.append(arg.constData());
    }

    data->unit.unit = clang_parseTranslationUnit(data->unit.index, filename.constData(),
                                                 clangArgs.data(), clangArgs.size(),
                                                 0, 0, CXTranslationUnit_None);
    if (data->unit.unit) {
        data->unit.file = clang_getFile(data->unit.unit, filename.constData());
        data->unit.origin = Source;
        initFileSystemWatcher(&data->unit);
        return true;
    } else {
        qWarning("failed to read unit from source: %s", filename.constData());
    }
    return false;
}

inline bool UnitCache::saveUnit(UnitData* data,
                                Resource* resource,
                                const QList<QByteArray>& arguments)
{
    const QByteArray hashedFilename = resource->hashedFileName(Resource::AST);
    const int result = clang_saveTranslationUnit(data->unit.unit,
                                                 hashedFilename.constData(),
                                                 CXSaveTranslationUnit_None);
    if (result == CXSaveError_None) {
        initFileSystemWatcher(&data->unit);
        resource->write(Resource::Information, arguments, Resource::Truncate);
        return true;
    }

    qWarning("Unable to save translation unit: %s (as %s)",
             data->unit.fileName.constData(), hashedFilename.constData());
    printDiagnostic(data->unit.unit, "save");
    return false;
}

inline void UnitCache::destroyUnit(UnitData* data)
{
    if (data->unit.unit)
        clang_disposeTranslationUnit(data->unit.unit);
    clang_disposeIndex(data->unit.index);
}

inline UnitCache::UnitStatus UnitCache::initUnit(const QByteArray& fileName,
                                                 const QList<QByteArray>& args,
                                                 int mode, UnitData* data)
{
    if ((mode & Source) || (mode & AST)) {
        QList<QByteArray> arguments = args;

        // we don't mind reread from disk
        if (!data->owner || (data->owner == QThread::currentThread() && !data->unit.unit)) {
            // and luckily we can
            if (!data->owner) {
                data->owner = QThread::currentThread();
                Q_ASSERT(!data->ref);
                ++data->ref;
            }

            // and we don't need to hold the mutex while doing so
            MutexUnlocker unlocker(&m_dataMutex);

            if (data->unit.unit) {
                // destroy the existing unit
                clang_disposeTranslationUnit(data->unit.unit);
                data->unit.unit = 0;
            }

            Resource resource(fileName);
            if (mode & AST) { // try to reread AST
                if (resource.exists(Resource::AST)) {
                    if (rereadUnit(resource.hashedFileName(Resource::AST), data)) {
                        // done!
                        return Done;
                    }
                }
                // Unable to read AST, get the .inf arguments if none are present
                if (arguments.isEmpty() && resource.exists(Resource::Information)) {
                    arguments = resource.read<QList<QByteArray> >(Resource::Information);
                    arguments.removeFirst(); // file name
                }
            }
            if (mode & Source) {
                if (loadUnit(fileName, arguments, data)) {
                    if (saveUnit(data, &resource, arguments)) {
                        // done!
                        return Done;
                    }
                }
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

UnitCache::Unit* UnitCache::createUnit(const QByteArray& fileName,
                                       const QList<QByteArray>& args,
                                       int mode)
{
    if (!mode)
        return 0;

    QMutexLocker locker(&m_dataMutex);
    for (;;) {
        const QHash<QByteArray, UnitData*>::iterator it = m_data.find(fileName);
        if (it != m_data.end()) {
            // the unit exists in our cache
            if (mode & Memory) {
                // we want to use a unit from the cache
                if (it.value()->owner == QThread::currentThread() || !it.value()->owner) {
                    it.value()->owner = QThread::currentThread();
                    ++it.value()->ref;
                    it.value()->unit.visited = QDateTime::currentDateTime();
                    return &it.value()->unit;
                }
                if (mode == Memory) {
                    // and we really really want to use a unit in our cache
                    m_dataCondition.wait(&m_dataMutex);
                    continue; // recheck
                }
            }

            const UnitStatus status = initUnit(fileName, args, mode, it.value());

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
                data->unit.fileName = fileName;
                m_data[fileName] = data;

                data->unit.unit = 0;
                data->unit.index = clang_createIndex(0, 0);

                const UnitStatus status = initUnit(fileName, args, mode, data);

                if (status == Done) {
                    return &data->unit;
                } else if (status == Abort) {
                    destroyUnit(data);
                    m_data.remove(fileName);
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

void UnitCache::recompile(Unit* unit)
{
    Resource resource(unit->fileName);

    QList<QByteArray> args = resource.read<QList<QByteArray> >(Resource::Information);
    args.removeFirst(); // file name

    QVector<const char*> clangargs;
    foreach(const QByteArray& a, args) {
        clangargs.append(a.constData());
    }

    CXTranslationUnit newunit;
    newunit = clang_parseTranslationUnit(unit->index,
                                         unit->fileName.constData(),
                                         clangargs.data(), clangargs.size(),
                                         0, 0,
                                         CXTranslationUnit_None);
    if (newunit) {
        const QByteArray fn = resource.hashedFileName(Resource::AST);
        int result = clang_saveTranslationUnit(newunit,
                                               fn.constData(),
                                               CXSaveTranslationUnit_None);
        if (result == CXSaveError_None) {
            clang_disposeTranslationUnit(unit->unit);
            unit->unit = newunit;
            unit->origin = Source;
            unit->visited = QDateTime::currentDateTime();
            unit->file = clang_getFile(unit->unit,
                                       unit->fileName.constData());
            initFileSystemWatcher(unit);

            Indexer::instance()->index(unit->fileName, args);
        } else {
            qWarning("Unable to save translation unit (3): %s (as %s)",
                     unit->fileName.constData(), fn.constData());
            printDiagnostic(newunit, "save (3)");
            clang_disposeTranslationUnit(newunit);
        }
    } else
        qWarning("Unable to recompile translation unit %s",
                 unit->fileName.constData());
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

static void findIncludes(CXFile includedFile, CXSourceLocation*, unsigned, CXClientData userData)
{
    CXString fn = clang_getFileName(includedFile);
    const char *cstr = clang_getCString(fn);
    if (strncmp(cstr, "/usr/", 5)) {
        QHash<Path, QSet<QByteArray> > &paths = *reinterpret_cast<QHash<Path, QSet<QByteArray> > *>(userData);
        Path p(cstr);
        p.resolve();
        paths[p.parentDir()].insert(p.fileName());
    }
    clang_disposeString(fn);
}

void UnitCache::initFileSystemWatcher(Unit* unit) // always called with m_dataMutex held
{
    QHash<Path, QSet<QByteArray> > paths;
    clang_getInclusions(unit->unit, findIncludes, &paths);
    // qDebug() << paths.keys();
    FileSystemWatcher *old = m_watchers.take(unit->fileName);
    if (!paths.isEmpty()) {
        FileSystemWatcher* watcher = new FileSystemWatcher(unit->fileName);
        watcher->moveToThread(thread());
        m_watchers[unit->fileName] = watcher;
        QStringList dirs;
        for (QHash<Path, QSet<QByteArray> >::iterator it = paths.begin(); it != paths.end(); ++it) {
            dirs.append(it.key());
        }
        watcher->paths = paths;
        watcher->addPaths(dirs);
        qDebug() << "adding" << paths << "for" << unit->fileName << dirs << old << watcher;
        connect(watcher, SIGNAL(directoryChanged(QString)),
                this, SLOT(onDirectoryChanged(QString)));
    }
    delete old;
}

void UnitCache::onDirectoryChanged(const QString &directory)
{
    const Path dir(directory.toLocal8Bit());
    qDebug() << "got dir changed" << dir;
    QMutexLocker lock(&m_dataMutex);
    FileSystemWatcher *f = qobject_cast<FileSystemWatcher*>(sender());
    if (f) {
        bool dirty = false;
        const QSet<QByteArray> &fileNames = f->paths.value(dir);
        qDebug() << f->paths << dir;
        Q_ASSERT(!fileNames.isEmpty());
        const QByteArray dirName = dir + "/";
        foreach(const QByteArray &fn, fileNames) {
            const Path p(dirName + fn);
            if (p.lastModified() > f->lastModified) {
                qDebug() << "recompiling" << f->fileName << "since lastModified for dir was"
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
