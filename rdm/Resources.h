#ifndef RESOURCELOCK_H
#define RESOURCELOCK_H

#include <QByteArray>
#include <QWaitCondition>
#include <QMutex>
#include <QHash>

class QThread;

class Resources
{
public:
    ~Resources();

    static Resources* instance();

    void lock(const QByteArray& resource);
    void unlock(const QByteArray& resource);
    bool tryLock(const QByteArray& resource);

private:
    Resources();

    QMutex m_mutex;
    QWaitCondition m_cond;
    struct Data
    {
        Data() : owner(0), ref(0) { }
        Data(const QThread* o) : owner(o), ref(1) { }
        const QThread* owner;
        int ref;
    };
    QHash<QByteArray, Data> m_resources;

    static Resources* s_inst;
};

class ResourceLocker
{
public:
    ResourceLocker(const QByteArray& resource) : m_res(resource) { Resources::instance()->lock(m_res); }
    ~ResourceLocker() { Resources::instance()->unlock(m_res); }

private:
    QByteArray m_res;
};

#endif // RESOURCELOCK_H
