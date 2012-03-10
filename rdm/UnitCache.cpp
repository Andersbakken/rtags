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

UnitCache::Unit* UnitCache::createUnit(const QByteArray& fileName,
                                       const QList<QByteArray>& arguments,
                                       int mode)
{
    if (!mode)
        return 0;

    QMutexLocker locker(&m_dataMutex);
    QList<QByteArray> argcopy = arguments;
    for (;;) {
        const QHash<QByteArray, UnitData*>::iterator it = m_data.find(fileName);
        if (it != m_data.end()) {
            // the unit exists in our cache
            if (mode == Memory) {
                if (it.value()->owner == QThread::currentThread() || !it.value()->owner) {
                    it.value()->owner = QThread::currentThread();
                    ++it.value()->ref;
                    return &it.value()->unit;
                }
                m_dataCondition.wait(&m_dataMutex);
                continue; // recheck
            }
            if (it.value()->owner == QThread::currentThread() && (mode & (AST | Force)) == AST) {
                // the unit is owned by our thread and we didn't explicitly request a reparse/reread
                ++it.value()->ref;
                it.value()->unit.visited = QDateTime::currentDateTime();
                return &it.value()->unit;
            } else if (!it.value()->owner) {
                // the unit is not owned by anyone
                UnitData* data = it.value();
                Q_ASSERT(!data->ref);

                data->owner = QThread::currentThread();
                ++data->ref;
                data->unit.visited = QDateTime::currentDateTime();

                bool loaded = false;
                if ((mode & (AST | Force)) == (AST | Force)) { // did we explicitly request a reread?
                    clang_disposeTranslationUnit(data->unit.unit);

                    Resource resource(fileName);
                    locker.unlock(); // no need to hold the lock while clang is parsing

                    if (resource.exists(Resource::AST)) {
                        const QByteArray fn = resource.hashedFileName(Resource::AST);
                        data->unit.unit = clang_createTranslationUnit(data->unit.index,
                                                                      fn.constData());
                        if (!data->unit.unit) {
                            if (!(mode & Source) && resource.exists(Resource::Information)) {
                                argcopy = resource.read<QList<QByteArray> >(Resource::Information);
                                argcopy.removeFirst(); // file name
                                mode |= Source;
                            }
                        } else {
                            loaded = true;
                            data->unit.origin = AST;
                            initFileSystemWatcher(&data->unit);
                        }
                    }

                    locker.relock();
                }

                if (!loaded && (mode & Source)) { // did we explicitly request a reparse?
                    locker.unlock(); // no need to hold the lock while clang is parsing
                    clang_disposeTranslationUnit(data->unit.unit);

                    QVector<const char*> args;
                    foreach(const QByteArray& a, argcopy) {
                        args.append(a.constData());
                    }

                    data->unit.unit = clang_parseTranslationUnit(data->unit.index,
                                                                 fileName.constData(),
                                                                 args.data(), args.size(),
                                                                 0, 0,
                                                                 CXTranslationUnit_None);
                    locker.relock();
                    if (!data->unit.unit) {
                        // double ow
                        qWarning("Unable to reparse an existing unit: %s",
                                 fileName.constData());
                        clang_disposeIndex(data->unit.index);
                        delete data;
                        m_data.erase(it);
                        m_dataCondition.wakeAll();
                        return 0;
                    }
                    data->unit.file = clang_getFile(data->unit.unit,
                                                    fileName.constData());
                    data->unit.origin = Source;

                    argcopy.prepend(fileName);
                    Resource resource(fileName);
                    QByteArray outfileName = resource.hashedFileName(Resource::AST);
                    int result = clang_saveTranslationUnit(data->unit.unit,
                                                           outfileName.constData(),
                                                           CXSaveTranslationUnit_None);
                    if (result == CXSaveError_None) {
                        initFileSystemWatcher(&data->unit);
                        resource.write(Resource::Information, argcopy, Resource::Truncate);
                    } else {
                        qWarning("Unable to save translation unit (1): %s (as %s)",
                                 fileName.constData(), outfileName.constData());
                        printDiagnostic(data->unit.unit, "save (1)");
                        clang_disposeTranslationUnit(data->unit.unit);
                        clang_disposeIndex(data->unit.index);
                        delete data;
                        m_data.erase(it);
                        m_dataCondition.wakeAll();
                        return 0;
                    }
                }
                return &data->unit;
            } else {
                // the unit is owned by someone else, wait for it to become available
                m_dataCondition.wait(&m_dataMutex);
                continue; // recheck
            }
        } else {
            if (mode == Memory)
                return 0;
            // the unit does not exist in our cache
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

                locker.unlock(); // no need to hold the lock while clang is parsing

                data->unit.unit = 0;
                data->unit.index = clang_createIndex(0, 0);
                bool loaded = false;
                if (mode & AST) {
                    Resource resource(fileName);
                    if (resource.exists(Resource::AST)) {
                        // we did not request a reparse and the unit exists as an AST file
                        const QByteArray fn = resource.hashedFileName(Resource::AST);
                        qDebug("reading %s (%s)", fn.constData(), fileName.constData());
                        data->unit.unit = clang_createTranslationUnit(data->unit.index,
                                                                      fn.constData());
                        if (!data->unit.unit) {
                            qDebug("unable to read unit?");
                            if (!(mode & Source) && resource.exists(Resource::Information)) {
                                qDebug("but we have info so we might try to reparse");
                                argcopy = resource.read<QList<QByteArray> >(Resource::Information);
                                argcopy.removeFirst(); // file name
                                mode |= Source;
                            } else {
                                qDebug("no info will be used");
                            }
                        } else {
                            qDebug("unit loaded successfully");
                            loaded = true;
                            data->unit.origin = AST;
                            initFileSystemWatcher(&data->unit);
                        }
                    }
                }
                if (!loaded && (mode & Source)) {
                    qDebug() << "trying to reparse" << fileName << argcopy;
                    // we need to (re)parse
                    QVector<const char*> args;
                    foreach(const QByteArray& a, argcopy) {
                        args.append(a.constData());
                    }

                    data->unit.unit = clang_parseTranslationUnit(data->unit.index,
                                                                 fileName.constData(),
                                                                 args.data(), args.size(),
                                                                 0, 0,
                                                                 CXTranslationUnit_None);
                    if (data->unit.unit) {
                        Resource resource(fileName);
                        const QByteArray fn = resource.hashedFileName(Resource::AST);
                        int result = clang_saveTranslationUnit(data->unit.unit,
                                                               fn.constData(),
                                                               CXSaveTranslationUnit_None);
                        if (result == CXSaveError_None) {
                            qDebug("Parsed %s successfully", fileName.constData());
                            Indexer::instance()->index(fileName, argcopy);
                            argcopy.prepend(fileName);
                            initFileSystemWatcher(&data->unit);
                            resource.write(Resource::Information, argcopy, Resource::Truncate);
                        } else {
                            qWarning("Unable to save translation unit (2): %s (as %s)",
                                     fileName.constData(), fn.constData());
                            printDiagnostic(data->unit.unit, "save (2)");
                            locker.relock();

                            clang_disposeTranslationUnit(data->unit.unit);
                            clang_disposeIndex(data->unit.index);
                            delete data;
                            m_data.remove(fileName);
                            m_dataCondition.wakeAll();
                            return 0;
                        }
                    } else {
                        qWarning("Couldn't parse %s", fileName.constData());
                    }
                    data->unit.origin = Source;
                }

                locker.relock();

                if (!data->unit.unit) {
                    // the unit could not be loaded, out of date? missing? parse errors?
                    clang_disposeIndex(data->unit.index);
                    delete data;
                    m_data.remove(fileName);
                    m_dataCondition.wakeAll();
                    return 0;
                }

                data->unit.file = clang_getFile(data->unit.unit,
                                                fileName.constData());

                return &data->unit;
            }
        }
    }

    Q_ASSERT_X(false, "UnitCache::acquire", "Should not reach this point");
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
        QHash<Path, QList<QByteArray> > &paths = *reinterpret_cast<QHash<Path, QList<QByteArray> > *>(userData);
        Path p(cstr);
        p.resolve();
        paths[p.parentDir()].append(p.fileName());
    }
    clang_disposeString(fn);
}

void UnitCache::initFileSystemWatcher(Unit* unit) // always called with m_dataMutex held
{
    QHash<Path, QList<QByteArray> > paths;
    clang_getInclusions(unit->unit, findIncludes, &paths);
    // qDebug() << paths.keys();
    FileSystemWatcher *old = m_watchers.take(unit->fileName);
    if (!paths.isEmpty()) {
        FileSystemWatcher* watcher = new FileSystemWatcher(unit->fileName);
        watcher->moveToThread(thread());
        m_watchers[unit->fileName] = watcher;
        QStringList dirs;
        for (QHash<Path, QList<QByteArray> >::iterator it = paths.begin(); it != paths.end(); ++it) {
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
        const QList<QByteArray> &fileNames = f->paths.value(dir);
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
