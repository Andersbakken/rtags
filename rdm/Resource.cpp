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

Resource::Resource(const QByteArray& fileName, LockMode mode)
    : m_fileName(fileName), m_locked(false)
{
    m_hash = m_fileName.isEmpty() ? QByteArray() : SHA256::hash(m_fileName);

    if (mode == Lock)
        lock();
}

Resource::~Resource()
{
    unlock();
}

QByteArray Resource::hashedFileName(Type type) const
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

QByteArray Resource::fileName() const
{
    return m_fileName;
}

void Resource::setFileName(const QByteArray& fileName, LockMode mode)
{
    if (m_fileName == fileName)
        return;

    unlock();

    m_fileName = fileName;
    m_hash = m_fileName.isEmpty() ? QByteArray() : SHA256::hash(m_fileName);

    if (mode == Lock)
        lock();
}

bool Resource::exists(Type type) const
{
    return QFile::exists(hashedFileName(type));
}

void Resource::setBaseDirectory(const QByteArray& base)
{
    s_base = base;
}

void Resource::erase(Type type)
{
    if (!m_locked)
        lock();
    Q_ASSERT(m_locked);

    QFile::remove(hashedFileName(type));
}

void Resource::eraseAll()
{
    if (!m_locked)
        lock();
    Q_ASSERT(m_locked);

    QFile::remove(hashedFileName(AST));
    QFile::remove(hashedFileName(Information));
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
    QFile file(hashedFileName(type));
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
    QFile file(hashedFileName(type));
    QIODevice::OpenMode qmode = QIODevice::WriteOnly;
    if (mode & Truncate)
        qmode |= QIODevice::Truncate;
    const bool ok = file.open(qmode);
    Q_ASSERT(ok);
    if (!ok)
        return;
    file.write(data);
}

QByteArray Resource::hash(const QByteArray& fileName)
{
    return SHA256::hash(fileName);
}
