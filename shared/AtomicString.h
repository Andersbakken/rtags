#ifndef AtomicString_h
#define AtomicString_h

#include <clang-c/Index.h>
#include <QByteArray>
#include <QHash>
#ifndef REENTRANT_ATOMICSTRING
// #define REENTRANT_ATOMICSTRING
#endif

class AtomicString
{
public:
    AtomicString() : mData(0) {}
    AtomicString(const CXString& string);
    AtomicString(const QByteArray& string);
    AtomicString(const QString& string);
    AtomicString(const AtomicString& other);
    AtomicString(const char* string, int size = -1);
    ~AtomicString();

    AtomicString& operator=(const AtomicString& other);
    QByteArray operator*() const { return mData ? mData->data : QByteArray(); }
    bool operator==(const AtomicString& other) const { return mData == other.mData; }
    bool operator==(const QByteArray& string) const { return mData ? mData->data == string : false; }
    bool operator!=(const QByteArray& string) const { return mData ? mData->data != string : false; }
    bool operator<(const QByteArray& string) const { return mData ? mData->data < string : false; }
    bool operator>(const QByteArray& string) const { return mData ? mData->data > string : false; }
    bool operator<=(const QByteArray& string) const { return mData ? mData->data <= string : false; }
    bool operator>=(const QByteArray& string) const { return mData ? mData->data >= string : false; }

    bool isEmpty() const { return mData ? mData->data.isEmpty() : true; }
    int strcmp(const AtomicString& other) const;

    QByteArray toByteArray() const { return mData ? mData->data : QByteArray(); }
    QString toString() const { return QString::fromUtf8(mData ? mData->data.constData() : 0); }
    const char* constData() const { return mData ? mData->data.constData() : 0; }

    int size() const { return mData ? mData->data.size() : 0; }
    void clear();
private:
    void init(const QByteArray& str);

private:
    struct Data
    {
        QByteArray data;
        int ref;
    };

    Data* mData;

    static QHash<QByteArray, Data*> sData;
#ifdef REENTRANT_ATOMICSTRING
    static QMutex sMutex;
#endif
};

static inline QDataStream &operator<<(QDataStream &ds, const AtomicString &string)
{
    ds << string.toByteArray();
    return ds;
}
static inline QDataStream &operator>>(QDataStream &ds, AtomicString &string)
{
    QByteArray tmp;
    ds >> tmp;
    string = tmp;
    return ds;
}

static inline uint qHash(const AtomicString& string)
{
    // ### consider storing the hash value
    return qHash(string.toByteArray());
}

static inline QDebug &operator<<(QDebug &dbg, const AtomicString &string)
{
    dbg << string.toByteArray();
    return dbg;
}

#endif
