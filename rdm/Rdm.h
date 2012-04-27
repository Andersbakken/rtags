#ifndef Rdm_h
#define Rdm_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include "Server.h"
#include <leveldb/db.h>
#include <leveldb/write_batch.h>
#include <RTags.h>
#include "Location.h"

class CursorInfo;
namespace Rdm {
enum { DatabaseVersion = 4 };
enum ReferenceType {
    NormalReference,
    MemberFunction,
    GlobalFunction
};
QByteArray eatString(CXString str);
QByteArray cursorToString(CXCursor cursor);
void initSystemPaths(const QList<Path> &paths);
bool isSystem(const Path &path);
template <typename T>
static inline bool startsWith(const QList<T> &list, const T &str)
{
    if (!list.isEmpty()) {
        //qDebug() << "filtering" << list << str;
        typename QList<T>::const_iterator it = qUpperBound(list, str);
        if (it != list.end()) {
            const int cmp = strncmp(str.constData(), (*it).constData(), (*it).size());
            if (cmp == 0) {
                return true;
            } else if (cmp < 0 && it != list.begin() && str.startsWith(*(it - 1))) {
                return true;
            }
        } else if (str.startsWith(*(it - 1))) {
            return true;
        }
    }
    return false;
}

static inline bool contains(leveldb::DB *db, const char *key)
{
    std::string str;
    return db->Get(leveldb::ReadOptions(), key, &str).ok();
}

template <typename T> T readValue(leveldb::DB *db, const char *key, bool *ok = 0)
{
    T t;
    std::string value;
    const leveldb::Status s = db->Get(leveldb::ReadOptions(), key, &value);
    if (!value.empty()) {
        const QByteArray v = QByteArray::fromRawData(value.c_str(), value.length());
        QDataStream ds(v);
        ds >> t;
    }
    if (ok)
        *ok = s.ok();
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

template <typename T> int writeValue(leveldb::WriteBatch *batch, const char *key, const T &t)
{
    Q_ASSERT(batch);
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << t;
    }
    batch->Put(key, leveldb::Slice(out.constData(), out.size()));
    return out.size();
}

template <typename T> int writeValue(leveldb::DB *db, const char *key, const T &t)
{
    Q_ASSERT(db);
    Q_ASSERT(key);
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << t;
    }
    db->Put(leveldb::WriteOptions(), leveldb::Slice(key, strlen(key)),
            leveldb::Slice(out.constData(), out.size()));
    return out.size();
}

CursorInfo findCursorInfo(leveldb::DB *db, const Location &key, Location *loc = 0);
}

#endif
