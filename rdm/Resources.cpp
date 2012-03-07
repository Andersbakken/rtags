#include "Resources.h"
#include <QMutexLocker>
#include <QThread>

Resources* Resources::s_inst = 0;

Resources::Resources()
{
}

Resources::~Resources()
{
    QMutexLocker locker(&m_mutex);
    m_resources.clear();
    m_cond.wakeAll();
}

Resources* Resources::instance()
{
    if (!s_inst)
        s_inst = new Resources;
    return s_inst;
}

void Resources::lock(const QByteArray& resource)
{
    const QThread* current = QThread::currentThread();

    QMutexLocker locker(&m_mutex);
    QHash<QByteArray, Data>::iterator it;
    for (;;) {
        it = m_resources.find(resource);
        if (it == m_resources.end())
            break;
        Q_ASSERT(it.value().owner != 0 && it.value().ref > 0);
        if (it.value().owner == current) {
            ++it.value().ref;
            return;
        }
        m_cond.wait(&m_mutex);
    }
    Q_ASSERT(!m_resources.contains(resource));
    m_resources[resource] = Data(current);
}

void Resources::unlock(const QByteArray& resource)
{
    const QThread* current = QThread::currentThread();
    Q_UNUSED(current) // shut up GCC when building release

    QMutexLocker locker(&m_mutex);
    Q_ASSERT(m_resources.contains(resource));
    QHash<QByteArray, Data>::iterator it = m_resources.find(resource);
    Q_ASSERT(it.value().owner == current && it.value().ref > 0);
    if (!--it.value().ref) {
        m_resources.erase(it);
        m_cond.wakeAll();
    }
}

bool Resources::tryLock(const QByteArray& resource)
{
    const QThread* current = QThread::currentThread();

    QMutexLocker locker(&m_mutex);
    const QHash<QByteArray, Data>::iterator it = m_resources.find(resource);
    if (it != m_resources.end()) {
        if (it.value().owner != current)
            return false;
        ++it.value().ref;
        return true;
    }

    m_resources[resource] = Data(current);
    return true;
}
