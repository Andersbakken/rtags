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
    : mFileSize(0), mFileUsed(0), mOffset(0), mPageOffset(0), mPageNo(0)
{
}

Mmap::Mmap(const QByteArray& filename)
    : mFileSize(0), mFileUsed(0), mOffset(0), mPageOffset(0), mPageNo(0)
{
    load(filename);
}

Mmap::~Mmap()
{
    clear(Async);
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

bool Mmap::load(const QByteArray& filename)
{
    if (filename.isEmpty()) {
        mError = "Filename is empty";
        return false;
    }

    if (filename == mFileName)
        clear(Sync);
    else
        clear(Async);
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

    const int pagesize = sPageSize * 10;

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
        if (ftruncate(fd, trunc) == -1) {
            close(fd);
            mError = "Failed to truncate " + mFileName;
            return false;
        }
        fileSize = trunc;
    }

    const quint64 numBlocks = fileSize / pagesize;
    const quint64 remBytes = fileSize % pagesize;

    quint64 offset = 0;
    Page data;
    for (quint64 i = 0; i < numBlocks; ++i) {
        data.size = pagesize;
        data.data = (char*)mmap(0, pagesize, PROT_READ | PROT_WRITE, MAP_SHARED, fd, offset);
        if (data.data == (void*)-1) {
            close(fd);
            clear(Async);
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
        clear(Async);
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

    int ret;
    memcpy(mPages.at(0).data, &mFileUsed, sizeof(int));
    foreach(const Page& data, mPages) {
        ret = msync(data.data, data.size, (sync == Async) ? MS_ASYNC : MS_SYNC);
        Q_ASSERT(ret == 0);
        ret = munmap(data.data, data.size);
        Q_ASSERT(ret == 0);
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
    if (size < mFileSize)
        return;

    const unsigned int off = mOffset;
    clear(Sync);
    reload(size + qMin(qMax(static_cast<unsigned long>(mFileSize), s500k) * 2, s32m));
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
        ensureSize(offset);
        mPageNo = findPage(mOffset, &mPageOffset);
        Q_ASSERT(mPageNo != -1);
    }
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
    while (size) {
        max = qMax(size, page->size - mPageOffset);
        Q_ASSERT(mOffset + max <= mFileUsed);
        memcpy(data + off, page->data + mPageOffset, max);
        mPageOffset = 0;
        size -= max;
        off += max;
        ++mPageNo;
        mOffset += max;

        Q_ASSERT((unsigned int)mPages.size() > mPageNo);
        page = &mPages.at(mPageNo);
    }
    mPageOffset = max;
}

void Mmap::write(const char* data, unsigned int size)
{
    ensureSize(mOffset + size);

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
    while (size) {
        max = qMax(size, page->size - mPageOffset);
        memcpy(page->data + mPageOffset, data + off, max);
        mPageOffset = 0;
        size -= max;
        off += max;
        ++mPageNo;
        mOffset += max;

        Q_ASSERT((unsigned int)mPages.size() > mPageNo);
        page = &mPages.at(mPageNo);
    }
    mPageOffset = max;

    if (mOffset > mFileUsed)
        mFileUsed = mOffset;
}
