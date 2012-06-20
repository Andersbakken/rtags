#ifndef Rdm_h
#define Rdm_h

#include <ByteArray.h>
#include <QIODevice>
#include <clang-c/Index.h>
#include <Path.h>
#include <RTags.h>
#include <QDebug>
#include "Mutex.h"
#include "MutexLocker.h"
#include "Location.h"
#include "Log.h"
#include "ResponseMessage.h"
#include "Server.h"

class CursorInfo;
class CXStringScope
{
public:
    CXStringScope(CXString str)
        : string(str)
    {
    }

    ~CXStringScope()
    {
        clang_disposeString(string);
    }
    CXString string;
};

struct FileInformation {
    FileInformation(time_t lt = 0, const QList<ByteArray> &args = QList<ByteArray>())
        : lastTouched(lt), compileArgs(args)
    {}

    time_t lastTouched;
    QList<ByteArray> compileArgs;
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
enum { DatabaseVersion = 10 };

enum ReferenceType {
    NormalReference,
    MemberFunction,
    GlobalFunction
};
}

class Database;
typedef Hash<Location, CursorInfo> SymbolHash;
typedef Hash<Location, QPair<Location, Rdm::ReferenceType> > ReferenceHash;
typedef Hash<ByteArray, Set<Location> > SymbolNameHash;
typedef Hash<quint32, Set<quint32> > DependencyHash;
typedef QPair<ByteArray, time_t> WatchedPair;
typedef Hash<ByteArray, Location> PchUSRHash;
typedef Hash<Path, Set<WatchedPair> > WatchedHash;
typedef Hash<quint32, FileInformation> InformationHash;

inline std::size_t hash_value(const QPair<ByteArray, time_t> &pair)
{
    std::size_t h1 = hash_value(pair.first);
    std::size_t h2 = hash_value(pair.second);
    return ((h1 << 16) | (h1 >> 16)) ^ h2;
}

inline std::size_t hash_value(const QString &string)
{
    const QByteArray ba = string.toLocal8Bit();
    return hashString(ba.constData(), ba.size());
}

namespace Rdm {
static inline bool isPch(const QList<ByteArray> &args)
{
    const int size = args.size();
    bool nextIsX = false;
    for (int i=0; i<size; ++i) {
        const ByteArray &arg = args.at(i);
        if (nextIsX) {
            return (arg == "c++-header" || arg == "c-header");
        } else if (arg == "-x") {
            nextIsX = true;
        } else if (arg.startsWith("-x")) {
            const ByteArray rest = ByteArray(arg.constData() + 2, arg.size() - 2);
            return (rest == "c++-header" || rest == "c-header");
        }
    }
    return false;
}

static inline bool isReference(CXCursorKind kind)
{
    return (clang_isReference(kind) || (kind >= CXCursor_FirstExpr && kind <= CXCursor_LastExpr));
}

void setMaxMemoryUsage(quint64 max);
bool waitForMemory(int maxMs);
ByteArray eatString(CXString str);
ByteArray cursorToString(CXCursor cursor);
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

CursorInfo findCursorInfo(Database *db, const Location &key, Location *loc = 0);
int writeSymbolNames(SymbolNameHash &symbolNames);
int writeDependencies(const DependencyHash &dependencies);
int writePchDepencies(const Hash<Path, Set<quint32> > &pchDependencies);
int writeFileInformation(quint32 fileId, const QList<ByteArray> &args, time_t lastTouched);
int writePchUSRHashes(const Hash<Path, PchUSRHash> &hashes);
int writeSymbols(SymbolHash &symbols, const ReferenceHash &references, quint32 fileId);
int dirty(const Set<quint32> &dirtyFileIds);
QList<ByteArray> compileArgs(quint32 fileId);
// the symbols will be modified before writing and we don't want to detach so we
// work on a non-const reference
class LogObject : public QObject, public LogOutput
{
    Q_OBJECT
public:
    LogObject(Connection *conn, int level)
        : QObject(conn), LogOutput(level), mConnection(conn)
    {
        connect(conn, SIGNAL(disconnected()), conn, SLOT(deleteLater()));
    }

    virtual void log(const char *msg, int len)
    {
        const ByteArray out(msg, len);
        QMetaObject::invokeMethod(this, "onLog", Qt::QueuedConnection, Q_ARG(ByteArray, out));
    }
public slots:
    void onLog(const ByteArray &log)
    {
        ResponseMessage msg(log);
        mConnection->send(&msg);
    }
private:
    Connection *mConnection;
};

class EventObject : public QObject, public EventOutput
{
    Q_OBJECT
    Q_ENUMS(Type)
public:
    enum Type { CError = 0x40000000 };

    EventObject(Connection *conn, int level)
        : QObject(conn), EventOutput(level), mConnection(conn)
    {
        connect(conn, SIGNAL(disconnected()), conn, SLOT(deleteLater()));
    }

    virtual void log(const char *msg, int len)
    {
        const ByteArray out(msg, len);
        QMetaObject::invokeMethod(this, "onLog", Qt::QueuedConnection, Q_ARG(ByteArray, out));
    }

    static int typeForName(const ByteArray &name);

public slots:
    void onLog(const ByteArray &log)
    {
        ResponseMessage msg(log);
        mConnection->send(&msg);
    }

private:
    Connection *mConnection;
};
}
static inline QDebug operator<<(QDebug dbg, CXCursor cursor)
{
    dbg << Rdm::cursorToString(cursor).constData();
    return dbg;
}

static inline QDebug operator<<(QDebug dbg, CXCursorKind kind)
{
    dbg << Rdm::eatString(clang_getCursorKindSpelling(kind)).constData();
    return dbg;
}


#endif
