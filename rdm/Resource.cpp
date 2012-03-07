#include "Resource.h"
#include "Resources.h"
#include "SHA256.h"
#include <QFile>

QByteArray Resource::s_base;

static const char* const extensions[] = { ".inf", ".ast" };

Resource::Resource()
    : m_locked(false)
{
}

Resource::Resource(const QByteArray& filename, LockMode mode)
    : m_filename(filename), m_locked(false)
{
    m_hash = m_filename.isEmpty() ? QByteArray() : SHA256::hash(m_filename);

    if (mode == Lock)
        lock();
}

Resource::~Resource()
{
    unlock();
}

QByteArray Resource::hashedFilename(Type type) const
{
    return s_base + "/" + m_hash + extensions[type];
}

void Resource::lock()
{
    if (m_hash.isEmpty() || m_locked)
        return;
    Resources::instance()->lock(m_hash);
    m_locked = true;
}

void Resource::unlock()
{
    if (!m_locked)
        return;
    Q_ASSERT(!m_hash.isEmpty());
    Resources::instance()->unlock(m_hash);
    m_locked = false;
}

QByteArray Resource::filename() const
{
    return m_filename;
}

void Resource::setFilename(const QByteArray& filename, LockMode mode)
{
    if (m_filename == filename)
        return;

    unlock();
    
    m_filename = filename;
    m_hash = m_filename.isEmpty() ? QByteArray() : SHA256::hash(m_filename);

    if (mode == Lock)
        lock();
}

bool Resource::exists(Type type) const
{
    return QFile::exists(hashedFilename(type));
}

void Resource::setBaseDirectory(const QByteArray& base)
{
    s_base = base;
}

QByteArray Resource::readData(Type type) const
{
    if (!exists(type))
        return QByteArray();
    if (!m_locked) {
        Resource* that = const_cast<Resource*>(this);
        that->lock();
    }
    Q_ASSERT(m_locked);
    QFile file(hashedFilename(type));
    const bool ok = file.open(QFile::ReadOnly);
    Q_ASSERT(ok);
    if (!ok)
        return QByteArray();
    return file.readAll();
}

void Resource::writeData(Type type, const QByteArray& data, WriteMode mode)
{
    if (!m_locked)
        lock();
    Q_ASSERT(m_locked);
    QFile file(hashedFilename(type));
    QIODevice::OpenMode qmode = QIODevice::WriteOnly;
    if (mode & Truncate)
        qmode |= QIODevice::Truncate;
    const bool ok = file.open(qmode);
    Q_ASSERT(ok);
    if (!ok)
        return;
    file.write(data);
}

QByteArray Resource::hash(const QByteArray& filename)
{
    return SHA256::hash(filename);
}
