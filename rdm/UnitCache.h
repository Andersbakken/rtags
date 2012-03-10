#ifndef UNITCACHE_H
#define UNITCACHE_H

#include "Path.h"
#include <clang-c/Index.h>
#include <QByteArray>
#include <QDateTime>
#include <QList>
#include <QHash>
#include <QMutex>
#include <QWaitCondition>
#include <QFileSystemWatcher>
#include <QObject>
#include <QSet>

class QThread;
class FileSystemWatcher;
class UnitCache : public QObject
{
    Q_OBJECT
public:
    enum LoadMode { None = 0x0, Source = 0x1, AST = 0x2, Memory = 0x4, Force = 0x8 };

    ~UnitCache();

    static UnitCache* instance();

    struct Unit
    {
        Unit()
            : origin(None), index(0), file(0), unit(0)
        {}
        LoadMode origin;
        QByteArray fileName;
        CXIndex index;
        CXFile file;
        CXTranslationUnit unit;
        QDateTime visited;
    };

    Unit* acquire(const QByteArray& filename, int mode = AST);
    Unit* acquire(const QByteArray& filename, const QList<QByteArray>& arguments, int mode = Source);
    void release(Unit* unit);
    void recompile(Unit* unit);
private slots:
    void onDirectoryChanged(const QString &dir);
private:
    UnitCache();
    void initFileSystemWatcher(Unit* unit);
    QList<Unit*> todo;

    struct UnitData
    {
        Unit unit;
        QThread* owner;
        int ref;
    };

    void cleanup(UnitData* data);
    bool removeUnusedUnits(int num);
    Unit* createUnit(const QByteArray& filename, const QList<QByteArray>& arguments, int mode);

    QHash<QByteArray, UnitData*> m_data;
    QHash<Path, FileSystemWatcher*> m_watchers;
    QMutex m_dataMutex;
    QWaitCondition m_dataCondition;
    static UnitCache* s_inst;

    friend class CachedUnit;
};

class FileSystemWatcher : public QFileSystemWatcher
{
    Q_OBJECT
public:
    FileSystemWatcher(const Path& fn)
        : fileName(fn), lastModified(time(0))
    {}

    const Path fileName;
    const quint64 lastModified;
    QHash<Path, QSet<QByteArray> > paths;
};

class CachedUnit
{
public:
    CachedUnit(const QByteArray& filename, int mode = UnitCache::AST);
    CachedUnit(const QByteArray& filename, const QList<QByteArray>& arguments, int mode = UnitCache::Source);
    ~CachedUnit() { UnitCache::instance()->release(m_unit); }

    void adopt(UnitCache::Unit* unit);

    UnitCache::Unit* unit() const { return m_unit; }

private:
    UnitCache::Unit* m_unit;
};

inline CachedUnit::CachedUnit(const QByteArray& filename, int mode)
{
    m_unit = UnitCache::instance()->acquire(filename, mode);
}

inline CachedUnit::CachedUnit(const QByteArray& filename, const QList<QByteArray>& arguments, int mode)
{
    m_unit = UnitCache::instance()->acquire(filename, arguments, mode);
}

inline UnitCache::Unit* UnitCache::acquire(const QByteArray& filename, int mode)
{
    return createUnit(filename, QList<QByteArray>(), mode);
}

inline UnitCache::Unit* UnitCache::acquire(const QByteArray& filename,
                                           const QList<QByteArray>& arguments,
                                           int mode)
{
    return createUnit(filename, arguments, mode);
}

#endif // UNITCACHE_H
