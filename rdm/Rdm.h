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
struct FileInformation {
    FileInformation() : lastTouched(0) {}
    time_t lastTouched;
    QList<QByteArray> compileArgs;
};

static inline QDataStream &operator<<(QDataStream &ds, const FileInformation &ci)
{
    ds << static_cast<quint64>(ci.lastTouched) << ci.compileArgs;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, FileInformation &ci)
{
    quint64 lastTouched;
    ds >> lastTouched;
    ci.lastTouched = static_cast<time_t>(lastTouched);
    ds >> ci.compileArgs;
    return ds;
}

namespace Rdm {
enum { DatabaseVersion = 4 };

enum ReferenceType {
    NormalReference,
    MemberFunction,
    GlobalFunction
};
}

typedef QHash<Location, CursorInfo> SymbolHash;
typedef QHash<Location, QPair<Location, Rdm::ReferenceType> > ReferenceHash;
typedef QHash<QByteArray, QSet<Location> > SymbolNameHash;
typedef QHash<Path, QSet<Path> > DependencyHash;
typedef QPair<QByteArray, quint64> WatchedPair;
typedef QHash<QByteArray, Location> PchUSRHash;
typedef QHash<Path, QSet<WatchedPair> > WatchedHash;
typedef QHash<Path, FileInformation> InformationHash;

namespace Rdm {
void setMaxMemoryUsage(quint64 max);
bool waitForMemory(int maxMs);
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

template <typename Container, typename Value>
static inline bool addTo(Container &container, const Value &value)
{
    const int oldSize = container.size();
    container += value;
    return container.size() != oldSize;
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


class Batch
{
public:
    enum { BatchThreshold = 1024 * 1024 };
    Batch(leveldb::DB *d)
        : db(d), batchSize(0), totalWritten(0)
    {}

    ~Batch()
    {
        write();
    }

    void write()
    {
        if (batchSize) {
            // error("About to write %d bytes to %p", batchSize, db);
            db->Write(leveldb::WriteOptions(), &batch);
            totalWritten += batchSize;
            // error("Wrote %d (%d) to %p", batchSize, totalWritten, db);
            batchSize = 0;
            batch.Clear();
        }
    }

    template <typename T>
    int add(const char *key, const T &t)
    {
        const int written = Rdm::writeValue<T>(&batch, key, t);
        batchSize += written;
        if (batchSize >= BatchThreshold)
            write();
        return written;
    }

    leveldb::DB *db;
    leveldb::WriteBatch batch;
    int batchSize, totalWritten;
};

CursorInfo findCursorInfo(leveldb::DB *db, const Location &key, Location *loc = 0);
int writeSymbolNames(SymbolNameHash &symbolNames);
int writeDependencies(const DependencyHash &dependencies);
int writePchDepencies(const DependencyHash &pchDependencies);
int writeFileInformation(const InformationHash &fileInformation);
int writePchUSRHashes(const QHash<Path, PchUSRHash> &hashes);
int writeSymbols(SymbolHash &symbols, const ReferenceHash &references);
// the symbols will be modified before writing and we don't want to detach so we
// work on a non-const reference
}

#endif
