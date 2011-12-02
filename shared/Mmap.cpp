#include "Mmap.h"
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

Mmap::Mmap()
    : m_size(0), m_bpd(0)
{
}

Mmap::Mmap(const QByteArray& filename)
    : m_size(0), m_bpd(0)
{
    load(filename);
}

Mmap::~Mmap()
{
    clear();
}

bool Mmap::load(const QByteArray& filename)
{
    clear();

    const long int pageSize = sysconf(_SC_PAGESIZE);
    Q_ASSERT(pageSize >= 1);

    const unsigned int bytesPerBlock = 10 * pageSize;
    //qWarning("bpb is %u", bytesPerBlock);

    const int fd = open(filename.constData(), O_RDWR);
    if (fd == -1) {
        m_error = "Unable to open " + filename;
        return false;
    }

    struct stat sb;
    if (fstat(fd, &sb) == -1) {
        close(fd);
        m_error = "Unable to stat " + filename;
        return false;
    }

    const off_t fileSize = sb.st_size;
    if (fileSize == 0) {
        close(fd);
        m_error = "File is empty " + filename;
        return false;
    }

    const quint64 numBlocks = fileSize / bytesPerBlock;
    const quint64 remBytes = fileSize % bytesPerBlock;

    quint64 offset = 0;
    Data data;
    for (quint64 i = 0; i < numBlocks; ++i) {
        data.size = bytesPerBlock;
        data.data = (char*)mmap(0, bytesPerBlock, PROT_READ | PROT_WRITE, MAP_SHARED, fd, offset);
        if (data.data == (void*)-1) {
            close(fd);
            clear();
            m_error = "Unable to mmap (bpb) " + filename + ": " + QByteArray::number(bytesPerBlock) + " at " + QByteArray::number(offset) + ", error " + QByteArray(strerror(errno));
            return false;
        }
        m_data.append(data);
        offset += bytesPerBlock;
    }
    Q_ASSERT(offset + remBytes == (quint64)fileSize);

    data.size = remBytes;
    data.data = (char*)mmap(0, remBytes, PROT_READ | PROT_WRITE, MAP_SHARED, fd, offset);
    if (data.data == (void*)-1) {
        close(fd);
        clear();
        m_error = "Unable to mmap (rem) " + filename + ": " + QByteArray::number(remBytes) + " at " + QByteArray::number(offset) + ", error " + QByteArray(strerror(errno));
        return false;
    }
    m_data.append(data);
    close(fd);

    m_size = fileSize;
    m_bpd = bytesPerBlock;

    return true;
}

void Mmap::clear()
{
    int ret;
    foreach(const Data& data, m_data) {
        ret = munmap(data.data, data.size);
        Q_ASSERT(ret == 0);
    }
    m_data.clear();
    m_size = 0;
    m_bpd = 0;
    m_error.clear();
}

void Mmap::sync()
{
    int ret;
    foreach(const Data& data, m_data) {
        ret = msync(data.data, data.size, MS_ASYNC);
        Q_ASSERT(ret == 0);
    }
}

const QByteArray Mmap::data(quint64 offset, unsigned int size) const
{
    Q_ASSERT(offset + size <= m_size);

    const int blockOffset = offset % m_bpd;
    int blockNo = offset / m_bpd;
    Q_ASSERT(blockNo < m_data.size());

    const Data& d = m_data[blockNo];
    if (blockOffset + size < d.size) // yay, easy way out
        return QByteArray::fromRawData(d.data + blockOffset, size);

    // boo!
    QByteArray data(d.data + blockOffset, d.size - blockOffset);
    size -= (d.size - blockOffset);
    for (;;) {
        Q_ASSERT(blockNo + 1 < m_data.size());
        const Data& dn = m_data[++blockNo];
        if (size > dn.size) {
            data += QByteArray(dn.data, dn.size);
            size -= dn.size;
        } else {
            data += QByteArray(dn.data, size);
            break;
        }
    }
    return data;
}

void Mmap::update(quint64 offset, const char* data, unsigned int size)
{
    Q_ASSERT(offset + size <= m_size);

    const int blockOffset = offset % m_bpd;
    int blockNo = offset / m_bpd;
    Q_ASSERT(blockNo < m_data.size());

    Data& d = m_data[blockNo];
    if (blockOffset + size < d.size) { // yay, easy way out
        memcpy(d.data + blockOffset, data, size);
        return;
    }

    // boo!
    memcpy(d.data + blockOffset, data, d.size - blockOffset);
    size -= (d.size - blockOffset);
    data += (d.size - blockOffset);
    for (;;) {
        Q_ASSERT(blockNo + 1 < m_data.size());
        Data& dn = m_data[++blockNo];
        if (size > dn.size) {
            memcpy(dn.data, data, dn.size);
            size -= dn.size;
            data += dn.size;
        } else {
            memcpy(dn.data, data, size);
            break;
        }
    }
}
