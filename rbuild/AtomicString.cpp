#include "AtomicString.h"

QHash<QByteArray, AtomicString::Data*> AtomicString::sData;
#ifdef REENTRANT_ATOMICSTRING
QMutex AtomicString::sMutex;
#endif


inline void AtomicString::init(const QByteArray& str)
{
    QHash<QByteArray, Data*>::iterator it = sData.find(str);
    if (it != sData.end()) {
        mData = it.value();
        ++mData->ref;
    } else {
        mData = new Data;
        mData->data = str;
        mData->ref = 1;
        sData[str] = mData;
    }
}

AtomicString::AtomicString(const CXString& string)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    QByteArray ba(clang_getCString(string));
    init(ba);
}

AtomicString::AtomicString(const QByteArray& string)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    init(string);
}

AtomicString::AtomicString(const QString& string)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    init(string.toUtf8());
}

AtomicString::AtomicString(const char* string, int size)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    init(QByteArray(string, size));
}

AtomicString::AtomicString(const AtomicString& other)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    mData = other.mData;
    if (mData)
        ++mData->ref;
}

AtomicString::~AtomicString()
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    if (mData && !--mData->ref) {
        sData.remove(mData->data);
        delete mData;
        mData = 0;
    }
}

int AtomicString::strcmp(const AtomicString& other) const
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    if (!mData)
        return !other.mData ? 0 : -1;
    if (!other.mData)
        return 1;
    return qstrcmp(mData->data, other.mData->data);
}

AtomicString& AtomicString::operator=(const AtomicString& other)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    if (mData && !--mData->ref) {
        sData.remove(mData->data);
        delete mData;
    }
    mData = other.mData;
    if (mData)
        ++mData->ref;
    return *this;
}

