#include "Mmap.h"
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>
#include <errno.h>

unsigned long Mmap::s500k;
unsigned long Mmap::s32m;
unsigned long Mmap::sPageSize;

Mmap::Mmap()
    : mFileSize(0), mFileUsed(0), mOffset(0), mPageOffset(0), mPageNo(0), mMode(ReadOnly)
{
}

Mmap::Mmap(const QByteArray& filename, Mode mode)
    : mFileSize(0), mFileUsed(0), mOffset(0), mPageOffset(0), mPageNo(0)
{
    load(filename, mode);
}

Mmap::~Mmap()
{
    if (mMode == ReadWrite)
        clear(ASync);
}

void Mmap::init()
{
    sPageSize = sysconf(_SC_PAGESIZE);
    s500k = sPageSize;
    const unsigned long k500 = 512 * 1024;
    while (s500k < k500) {
        s500k += sPageSize;
    }
    s32m = s500k * 64;
}

bool Mmap::load(const QByteArray& filename, Mode mode)
{
    if (filename.isEmpty()) {
        mError = "Filename is empty";
        return false;
    }

    mMode = mode;

    if (filename == mFileName)
        clear(Sync);
    else
        clear(ASync);
    mFileName = filename;

    return reload();
}

bool Mmap::reload(unsigned int trunc)
{
    const int fd = open(mFileName.constData(), O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH);
    if (fd == -1) {
        mError = "Unable to open " + mFileName;
        return false;
    }

    const int pagesize = s500k;

    off_t fileSize = 0;
    if (!trunc) {
        struct stat sb;
        if (fstat(fd, &sb) == -1) {
            close(fd);
            mError = "Unable to stat " + mFileName;
            return false;
        }

        fileSize = sb.st_size;
        if (!fileSize) {
            // resize
            close(fd);
            return reload(s500k);
        }
    } else {
        //qDebug() << "trunc to" << trunc;
        Q_ASSERT(trunc >= s500k && !(trunc % sPageSize));
        if (ftruncate(fd, trunc) == -1) {
            close(fd);
            mError = "Failed to truncate " + mFileName;
            return false;
        }
        fileSize = trunc;
    }

    Q_ASSERT(!(fileSize % sPageSize));

    const quint64 numBlocks = fileSize / pagesize;
    const quint64 remBytes = fileSize % pagesize;

    int prot = PROT_READ;
    if (mMode == ReadWrite)
        prot |= PROT_WRITE;

    quint64 offset = 0;
    Page data;
    for (quint64 i = 0; i < numBlocks; ++i) {
        data.size = pagesize;
        data.data = (char*)mmap(0, pagesize, prot, MAP_SHARED, fd, offset);
        if (data.data == (void*)-1) {
            close(fd);
            clear(ASync);
            mError = "Unable to mmap (bpb) " + mFileName + ": " + QByteArray::number(pagesize) + " at " + QByteArray::number(offset) + ", error " + QByteArray(strerror(errno));
            return false;
        }
        mPages.append(data);
        offset += pagesize;
    }
    Q_ASSERT(offset + remBytes == (quint64)fileSize);

    data.size = remBytes;
    data.data = (char*)mmap(0, remBytes, PROT_READ | PROT_WRITE, MAP_SHARED, fd, offset);
    if (data.data == (void*)-1) {
        close(fd);
        clear(ASync);
        mError = "Unable to mmap (rem) " + mFileName + ": " + QByteArray::number(remBytes) + " at " + QByteArray::number(offset) + ", error " + QByteArray(strerror(errno));
        return false;
    }
    mPages.append(data);
    close(fd);

    mFileSize = fileSize;
    mOffset = sizeof(int); // first int is reserved for mFileUsed
    Q_ASSERT(mFileSize >= sizeof(int));
    mFileUsed = *reinterpret_cast<const int*>(mPages.at(0).data);
    if (!mFileUsed)
        mFileUsed = sizeof(int);
    seek(0);

    return true;
}

void Mmap::clear(SyncType sync)
{
    if (mPages.empty())
        return;

    if (sync != NoSync) {
        int ret;
        memcpy(mPages.at(0).data, &mFileUsed, sizeof(int));
        foreach(const Page& data, mPages) {
            ret = msync(data.data, data.size, (sync == ASync) ? MS_ASYNC : MS_SYNC);
            Q_ASSERT(ret == 0);
            ret = munmap(data.data, data.size);
            Q_ASSERT(ret == 0);
        }
    }
    mPages.clear();
    mError.clear();
    mFileSize = 0;
    mOffset = mPageOffset = 0;
    mFileUsed = 0;
    mPageNo = 0;
}

void Mmap::ensureSize(unsigned int size)
{
    if (size <= mFileSize)
        return;

    if ((size % sPageSize) != 0) {
        size -= (size % sPageSize);
        size += sPageSize;
    }

    const unsigned int off = mOffset;
    const unsigned int sz = mFileSize;
    clear(Sync);
    reload(size + qMin(qMax(static_cast<unsigned long>(sz), s500k) * 2, s32m));
    seek(off);
}

int Mmap::findPage(unsigned int fileOffset, unsigned int* pageOffset) const
{
    int curSize = 0, curPos = 0;
    foreach(const Page& data, mPages) {
        if (curSize + data.size > fileOffset) {
            Q_ASSERT(fileOffset >= static_cast<unsigned int>(curSize));
            if (pageOffset)
                *pageOffset = fileOffset - curSize;
            return curPos;
        }
        curSize += data.size;
        ++curPos;
    }
    return -1;
}

void Mmap::seek(unsigned int offset)
{
    if (offset == 0)
        offset = sizeof(int);
    mOffset = offset;
    mPageNo = findPage(mOffset, &mPageOffset);
    if (mPageNo == -1) {
        ensureSize(offset + 1);
        mPageNo = findPage(mOffset, &mPageOffset);
        if (mPageNo == -1)
            qFatal("Unable to find page at offset %u, file size is %u", offset, mFileSize);
    }
}

void Mmap::reset()
{
    clear(NoSync);
    reload(s500k);
    seek(0);
}

void Mmap::read(char* data, unsigned int size, bool* ok) const
{
    if (mOffset + size > mFileUsed) {
        if (ok)
            *ok = false;
        return;
    }

    if (ok)
        *ok = true;

    const Page* page = &mPages.at(mPageNo);
    if (mPageOffset + size < page->size) {
        memcpy(data, page->data + mPageOffset, size);
        mPageOffset += size;
        mOffset += size;
        if (mPageOffset == page->size - 1) {
            ++mPageNo;
            mPageOffset = 0;
        }
        return;
    }

    // need to copy in blocks
    unsigned int off = 0, max = 0;
    for (;;) {
        max = qMin(size, page->size - mPageOffset);
        memcpy(data + off, page->data + mPageOffset, max);
        mPageOffset += max;

        Q_ASSERT(mPageOffset <= page->size);
        Q_ASSERT(size >= max);
        size -= max;
        off += max;
        mOffset += max;

        if (mPageOffset == page->size) {
            mPageOffset = 0;
            ++mPageNo;

            Q_ASSERT((unsigned int)mPages.size() > mPageNo || !size);
            if (size)
                page = &mPages.at(mPageNo);
        }

        if (!size)
            break;
    }
}

void Mmap::write(const char* data, unsigned int size)
{
    if (mMode == ReadOnly)
        return;

    ensureSize(mOffset + size);

    Q_ASSERT(mPageNo < (unsigned int)mPages.size());

    const Page* page = &mPages.at(mPageNo);
    if (mPageOffset + size < page->size) {
        memcpy(page->data + mPageOffset, data, size);
        mPageOffset += size;
        mOffset += size;
        if (mPageOffset == page->size - 1) {
            ++mPageNo;
            mPageOffset = 0;
        }

        if (mOffset > mFileUsed)
            mFileUsed = mOffset;
        return;
    }

    unsigned int off = 0, max = 0;
    for (;;) {
        max = qMin(size, page->size - mPageOffset);
        memcpy(page->data + mPageOffset, data + off, max);
        mPageOffset += max;

        Q_ASSERT(mPageOffset <= page->size);
        Q_ASSERT(size >= max);
        size -= max;
        off += max;
        mOffset += max;

        if (mPageOffset == page->size) {
            mPageOffset = 0;
            ++mPageNo;

            Q_ASSERT((unsigned int)mPages.size() > mPageNo || !size);
            if (size)
                page = &mPages.at(mPageNo);
        }

        if (!size)
            break;
    }

    if (mOffset > mFileUsed)
        mFileUsed = mOffset;
}
