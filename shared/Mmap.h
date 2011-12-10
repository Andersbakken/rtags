#ifndef MMAP_H
#define MMAP_H

#include <QByteArray>
#include <QVector>
//#include <QDebug>

class Mmap
{
public:
    enum Mode { ReadOnly, ReadWrite };

    Mmap();
    Mmap(const QByteArray& filename, Mode mode);
    ~Mmap();

    static void init();

    bool load(const QByteArray& filename, Mode mode);

    void seek(unsigned int offset);
    void reset();

    Mode mode() const { return mMode; }
    unsigned int offset() const { return mOffset; }
    unsigned int size() const { return mFileUsed; }

    QByteArray fileName() const { return mFileName; }
    QByteArray lastError() const { return mError; }

    template<typename T>
    T get(bool* ok = 0) const;
    template<typename T>
    void set(const T& data);

private:
    enum SyncType { Sync, ASync, NoSync };
    void clear(SyncType sync);

    bool reload(unsigned int reset = 0);

    int findPage(unsigned int fileOffset, unsigned int* pageOffset) const;

    void ensureSize(unsigned int size);
    void read(char* data, unsigned int size, bool* ok) const;
    void write(const char* data, unsigned int size);

    Mmap(const Mmap& other);
    Mmap& operator=(const Mmap& other);

private:
    struct Page
    {
        char* data;
        unsigned int size;
    };
    QVector<Page> mPages;
    unsigned int mFileSize, mFileUsed;
    mutable unsigned int mOffset, mPageOffset;
    mutable int mPageNo;
    QByteArray mFileName, mError;
    Mode mMode;

    static unsigned long s500k, s32m, sPageSize;

    friend class MmapDevice;
};

template<typename T>
inline T Mmap::get(bool* ok) const
{
    T data;
    read(reinterpret_cast<char*>(&data), sizeof(T), ok);
}

template<typename T>
inline void Mmap::set(const T& data)
{
    write(&data, sizeof(T));
}

template<>
inline QByteArray Mmap::get<QByteArray>(bool* ok) const
{
    int size;
    bool iok;
    read(reinterpret_cast<char*>(&size), sizeof(int), &iok);
    if (!iok) {
        if (ok)
            *ok = false;
        return QByteArray();
    }

    /*const bool dodebug = mFileName.endsWith("/a.db");
    if (dodebug)
        qDebug() << "reading bytearray of size" << size << "at" << mOffset - sizeof(int) << "which brings our offset to" << mOffset + size;
    if (dodebug)
        qDebug() << "offset is now" << mOffset;*/

    Q_ASSERT(mPageNo < mPages.size());
    const Page* page = &mPages.at(mPageNo);
    if (mPageOffset + size <= page->size) { // do fromRawData() for efficiency
        const unsigned int off = mPageOffset;
        mPageOffset += size;
        if (mPageOffset == page->size) {
            ++mPageNo;
            mPageOffset = 0;
        }
        mOffset += size;
        //if (dodebug)
        //    qDebug() << "offset is now" << mOffset;
        if (ok)
            *ok = true;
        //return QByteArray::fromRawData(page->data + off, size);
        return QByteArray(page->data + off, size);
    }

    QByteArray data(size, Qt::Uninitialized);
    read(data.data(), size, &iok);
    //if (dodebug)
    //    qDebug() << "offset is now" << mOffset;
    //if (mFileName.endsWith("/a.db"))
    //    qDebug() << "read ok?" << iok;
    if (ok)
        *ok = iok;
    return iok ? data : QByteArray();
}

template<>
inline void Mmap::set<QByteArray>(const QByteArray& data)
{
    if (mMode == ReadOnly)
        return;

    int size = data.size();
    int oldsize = -1;

    if (mOffset + sizeof(int) < mFileUsed) {
        // assume that we're overwriting an existing QByteArray
        // if this is not the case then this will likely fail miserably
        const unsigned int off = offset();
        read(reinterpret_cast<char*>(&oldsize), sizeof(int), 0);
        seek(off);
        Q_ASSERT(oldsize >= size);
    }

    /*const bool dodebug = mFileName.endsWith("/a.db");
    if (dodebug)
        qDebug() << "writing bytearray of size" << (oldsize != -1 ? oldsize : size) << "at" << mOffset << "with filename" << mFileName;
    if (dodebug)
        qDebug() << "offset is now" << mOffset;*/
    write(reinterpret_cast<const char*>(oldsize != -1 ? &oldsize : &size), sizeof(int));
    //if (dodebug)
    //    qDebug() << "offset is now" << mOffset;
    write(data.constData(), size);
    //if (dodebug)
    //    qDebug() << "offset is now" << mOffset;
}

#endif // MMAP_H
