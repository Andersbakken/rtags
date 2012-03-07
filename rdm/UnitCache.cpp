#include "UnitCache.h"
#include "Resource.h"
#include "Indexer.h"
#include <QMutexLocker>
#include <QThread>
#include <QVector>
#include <QMap>

UnitCache* UnitCache::s_inst = 0;

static const int MaxUnits = 5;

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

UnitCache::Unit* UnitCache::createUnit(const QByteArray& filename,
                                       const QList<QByteArray>& arguments,
                                       int mode)
{
    if (!mode)
        return 0;

    QMutexLocker locker(&m_dataMutex);
    QList<QByteArray> argcopy = arguments;
    for (;;) {
        const QHash<QByteArray, UnitData*>::iterator it = m_data.find(filename);
        if (it != m_data.end()) {
            // the unit exists in our cache
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

                    Resource resource(filename);
                    locker.unlock(); // no need to hold the lock while clang is parsing

                    if (resource.exists(Resource::AST)) {
                        const QByteArray fn = resource.hashedFilename(Resource::AST);
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
                                                                 filename.constData(),
                                                                 args.data(), args.size(),
                                                                 0, 0,
                                                                 CXTranslationUnit_None);
                    locker.relock();
                    if (!data->unit.unit) {
                        // double ow
                        qWarning("Unable to reparse an existing unit: %s",
                                 filename.constData());
                        clang_disposeIndex(data->unit.index);
                        delete data;
                        m_data.erase(it);
                        m_dataCondition.wakeAll();
                        return 0;
                    }
                    data->unit.file = clang_getFile(data->unit.unit,
                                                    filename.constData());
                    data->unit.origin = Source;

                    argcopy.prepend(filename);
                    Resource resource(filename);
                    QByteArray outfilename = resource.hashedFilename(Resource::AST);
                    int result = clang_saveTranslationUnit(data->unit.unit,
                                                           outfilename.constData(),
                                                           CXSaveTranslationUnit_None);
                    if (result == CXSaveError_None)
                        resource.write(Resource::Information, argcopy, Resource::Truncate);
                    else {
                        qWarning("Unable to save translation unit (1): %s (as %s)",
                                 filename.constData(), outfilename.constData());
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
                data->unit.filename = filename;
                m_data[filename] = data;

                locker.unlock(); // no need to hold the lock while clang is parsing

                data->unit.unit = 0;
                data->unit.index = clang_createIndex(0, 0);
                bool loaded = false;
                if (mode & AST) {
                    Resource resource(filename);
                    if (resource.exists(Resource::AST)) {
                        // we did not request a reparse and the unit exists as an AST file
                        const QByteArray fn = resource.hashedFilename(Resource::AST);
                        qDebug("reading %s", fn.constData());
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
                        }
                    }
                }
                if (!loaded && (mode & Source)) {
                    qDebug("trying to reparse, %d arguments", argcopy.size());
                    // we need to (re)parse
                    QVector<const char*> args;
                    foreach(const QByteArray& a, argcopy) {
                        args.append(a.constData());
                    }

                    data->unit.unit = clang_parseTranslationUnit(data->unit.index,
                                                                 filename.constData(),
                                                                 args.data(), args.size(),
                                                                 0, 0,
                                                                 CXTranslationUnit_None);
                    if (data->unit.unit) {
                        Resource resource(filename);
                        const QByteArray fn = resource.hashedFilename(Resource::AST);
                        int result = clang_saveTranslationUnit(data->unit.unit,
                                                               fn.constData(),
                                                               CXSaveTranslationUnit_None);
                        if (result == CXSaveError_None) {
                            Indexer::instance()->index(filename, argcopy);
                            argcopy.prepend(filename);
                            resource.write(Resource::Information, argcopy, Resource::Truncate);
                        } else {
                            qWarning("Unable to save translation unit (2): %s (as %s)",
                                     filename.constData(), fn.constData());
                            locker.relock();

                            clang_disposeTranslationUnit(data->unit.unit);
                            clang_disposeIndex(data->unit.index);
                            delete data;
                            m_data.remove(filename);
                            m_dataCondition.wakeAll();
                            return 0;
                        }
                    }
                    data->unit.origin = Source;
                }

                locker.relock();

                if (!data->unit.unit) {
                    // the unit could not be loaded, out of date? missing? parse errors?
                    clang_disposeIndex(data->unit.index);
                    delete data;
                    m_data.remove(filename);
                    m_dataCondition.wakeAll();
                    return 0;
                }

                data->unit.file = clang_getFile(data->unit.unit,
                                                filename.constData());

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

    const QHash<QByteArray, UnitData*>::iterator it = m_data.find(unit->filename);
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
    Resource resource(unit->filename);

    QList<QByteArray> args = resource.read<QList<QByteArray> >(Resource::Information);
    args.removeFirst(); // file name

    QVector<const char*> clangargs;
    foreach(const QByteArray& a, args) {
        clangargs.append(a.constData());
    }

    CXTranslationUnit newunit;
    newunit = clang_parseTranslationUnit(unit->index,
                                         unit->filename.constData(),
                                         clangargs.data(), clangargs.size(),
                                         0, 0,
                                         CXTranslationUnit_None);
    if (newunit) {
        const QByteArray fn = resource.hashedFilename(Resource::AST);
        int result = clang_saveTranslationUnit(newunit,
                                               fn.constData(),
                                               CXSaveTranslationUnit_None);
        if (result == CXSaveError_None) {
            clang_disposeTranslationUnit(unit->unit);
            unit->unit = newunit;
            unit->origin = Source;
            unit->visited = QDateTime::currentDateTime();
            unit->file = clang_getFile(unit->unit,
                                       unit->filename.constData());

            Indexer::instance()->index(unit->filename, args);
        } else {
            qWarning("Unable to save translation unit (3): %s (as %s)",
                     unit->filename.constData(), fn.constData());
            clang_disposeTranslationUnit(newunit);
        }
    } else
        qWarning("Unable to recompile translation unit %s",
                 unit->filename.constData());
}

void CachedUnit::adopt(UnitCache::Unit* unit)
{
    if (unit == m_unit)
        return;

    UnitCache* cache = UnitCache::instance();
    cache->release(m_unit);
    if (unit)
        m_unit = cache->acquire(unit->filename);
    else
        m_unit = 0;
}
