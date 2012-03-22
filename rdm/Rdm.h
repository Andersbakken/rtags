#ifndef Tools_h
#define Tools_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include "UnitCache.h"
#include "Database.h"
#include <leveldb/db.h>
#include <leveldb/write_batch.h>
#include <RTags.h>

namespace Rdm {
QByteArray eatString(CXString str);
QByteArray cursorToString(CXCursor cursor);

struct CursorInfo {
    CursorInfo() : symbolLength(0), kind(CXCursor_FirstInvalid) {}
    bool isNull() const { return symbolLength; }
    int symbolLength;
    RTags::Location target;
    QSet<RTags::Location> references;
    CXCursorKind kind;
    bool unite(const CursorInfo &other)
    {
        if (!symbolLength) {
            *this = other;
            return true;
        }
        Q_ASSERT(target == other.target);
        
        const int oldSize = references.size();
        references.unite(other.references);
        return oldSize != references.size();
    }
};

static inline QDataStream &operator<<(QDataStream &ds, const CursorInfo &ci)
{
    ds << ci.symbolLength << ci.target << ci.references << static_cast<quint32>(ci.kind);
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, CursorInfo &ci)
{
    quint32 kind;
    ds >> ci.symbolLength >> ci.target >> ci.references >> kind;
    ci.kind = static_cast<CXCursorKind>(kind);
    return ds;
}

template <typename T> T readValue(leveldb::DB *db, const char *key)
{
    T t;
    std::string value;
    db->Get(leveldb::ReadOptions(), key, &value);
    if (!value.empty()) {
        const QByteArray v = QByteArray::fromRawData(value.c_str(), value.length());
        QDataStream ds(v);
        ds >> t;
    }
    return t;
}

template <typename T> T readValue(leveldb::Iterator *it)
{
    T t;
    leveldb::Slice value = it->value();
    const QByteArray v = QByteArray::fromRawData(value.data(), value.size());
    if (!v.isEmpty()) {
        QDataStream ds(v);
        ds >> t;
    }
    return t;
}

template <typename T> void writeValue(leveldb::WriteBatch *batch, const char *key, const T &t)
{
    Q_ASSERT(batch);
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << t;
    }
    batch->Put(key, leveldb::Slice(out.constData(), out.size()));
}

CursorInfo findCursorInfo(leveldb::DB *db, const RTags::Location &key);
}


#endif
