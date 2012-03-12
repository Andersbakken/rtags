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
class Resource;

class UnitCache : public QObject
{
    Q_OBJECT
public:
    // AST implies Info
    enum LoadFlag {
        None = 0x00,
        Info = 0x01,
        Source = 0x02,
        AST = 0x04,
        Memory = 0x08,
        NoLock = 0x10,
        Precompile = 0x20
    };

    ~UnitCache();

    static UnitCache* instance();

    struct Unit
    {
        Unit()
            : origin(None), index(0), file(0), unit(0), precompile(false)
        {}
        LoadFlag origin;
        QByteArray fileName;
        CXIndex index;
        CXFile file;
        CXTranslationUnit unit;
        QDateTime visited;
        QList<QByteArray> pchs;
        bool precompile;
    };

    Unit* acquire(const QByteArray& filename, int mode = AST | Memory);
    Unit* acquire(const QByteArray& filename, const QList<QByteArray>& arguments, int mode = Source | Memory);
    Unit* acquire(const QByteArray& input, const QByteArray& output,
                  const QList<QByteArray>& arguments, int mode = Source | Memory);
    void release(Unit* unit);

private slots:
    void onDirectoryChanged(const QString &dir);

private:
    UnitCache();
    void initFileSystemWatcher(Unit* unit);
    void freeFileSystemWatcher(const QByteArray& filename);

    QList<Unit*> todo;

    struct UnitData
    {
        Unit unit;
        QThread* owner;
        int ref;
    };
    enum UnitStatus { Done, Wait, Abort };

    void cleanup(UnitData* data);
    bool removeUnusedUnits(int num);
    bool recheckPch(const QList<QByteArray>& arguments, UnitData* data);
    bool rereadUnit(const QByteArray& hashedFilename, UnitData* data);
    bool loadUnit(const QByteArray& filename, const QList<QByteArray>& arguments, UnitData* data, bool initWatcher, bool *errors);
    enum SaveMode {
        SaveAST = 0x1,
        SaveInfo = 0x2
    };
    bool saveUnit(UnitData* data, Resource* resource, const QList<QByteArray>& arguments, unsigned flags);
    void destroyUnit(UnitData* data);
    UnitStatus initUnit(const QByteArray& input, const QByteArray& output,
                        const QList<QByteArray>& args, int mode, UnitData* data);

    Unit* createUnit(const QByteArray& input, const QByteArray& output,
                     const QList<QByteArray>& arguments, int mode);

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
    CachedUnit(const QByteArray& filename, int mode = UnitCache::AST | UnitCache::Memory);
    CachedUnit(const QByteArray& filename, const QList<QByteArray>& arguments,
               int mode = UnitCache::Source | UnitCache::Memory);
    CachedUnit(const QByteArray& input, const QByteArray& output, const QList<QByteArray>& arguments,
               int mode = UnitCache::Source | UnitCache::Memory);
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

inline CachedUnit::CachedUnit(const QByteArray& input, const QByteArray& output,
                              const QList<QByteArray>& arguments, int mode)
{
    m_unit = UnitCache::instance()->acquire(input, output, arguments, mode);
}

inline UnitCache::Unit* UnitCache::acquire(const QByteArray& filename, int mode)
{
    return createUnit(filename, filename, QList<QByteArray>(), mode);
}

inline UnitCache::Unit* UnitCache::acquire(const QByteArray& filename,
                                           const QList<QByteArray>& arguments,
                                           int mode)
{
    return createUnit(filename, filename, arguments, mode);
}

inline UnitCache::Unit* UnitCache::acquire(const QByteArray& input, const QByteArray& output,
                                           const QList<QByteArray>& arguments,
                                           int mode)
{
    return createUnit(input, output, arguments, mode);
}

#endif // UNITCACHE_H
