#include "MmapDevice.h"
#include "Mmap.h"

MmapDevice::MmapDevice(Mmap* mmap, QObject *parent)
    : QIODevice(parent), mMmap(mmap)
{
    QIODevice::open(QIODevice::ReadWrite);
}

bool MmapDevice::isSequential() const
{
    return true;
}

bool MmapDevice::atEnd() const
{
    return (mMmap->offset() == mMmap->size());
}

qint64 MmapDevice::readData(char* data, qint64 maxlen)
{
    const qint64 total = qMin(maxlen, static_cast<qint64>(mMmap->size() - mMmap->offset()));
    mMmap->read(data, total, 0);
    return total;
}

qint64 MmapDevice::writeData(const char* data, qint64 len)
{
    mMmap->write(data, len);
    return len;
}
