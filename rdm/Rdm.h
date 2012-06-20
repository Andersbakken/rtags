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
    FileInformation(time_t lt = 0, const List<ByteArray> &args = List<ByteArray>())
        : lastTouched(lt), compileArgs(args)
    {}

    time_t lastTouched;
    List<ByteArray> compileArgs;
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
typedef Map<Location, CursorInfo> SymbolMap;
typedef Map<Location, QPair<Location, Rdm::ReferenceType> > ReferenceMap;
typedef Map<ByteArray, Set<Location> > SymbolNameMap;
typedef Map<uint32_t, Set<uint32_t> > DependencyMap;
typedef QPair<ByteArray, time_t> WatchedPair;
typedef Map<ByteArray, Location> PchUSRMap;
typedef Map<Path, Set<WatchedPair> > WatchedMap;
typedef Map<uint32_t, FileInformation> InformationMap;

namespace Rdm {
static inline bool isPch(const List<ByteArray> &args)
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

void setMaxMemoryUsage(uint64_t max);
bool waitForMemory(int maxMs);
ByteArray eatString(CXString str);
ByteArray cursorToString(CXCursor cursor);
template <typename T>
static inline bool startsWith(const List<T> &list, const T &str)
{
    if (!list.isEmpty()) {
        //qDebug() << "filtering" << list << str;
        typename List<T>::const_iterator it = std::upper_bound(list.begin(), list.end(), str);
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
int writeSymbolNames(SymbolNameMap &symbolNames);
int writeDependencies(const DependencyMap &dependencies);
int writePchDepencies(const Map<Path, Set<uint32_t> > &pchDependencies);
int writeFileInformation(uint32_t fileId, const List<ByteArray> &args, time_t lastTouched);
int writePchUSRMapes(const Map<Path, PchUSRMap> &hashes);
int writeSymbols(SymbolMap &symbols, const ReferenceMap &references, uint32_t fileId);
int dirty(const Set<uint32_t> &dirtyFileIds);
List<ByteArray> compileArgs(uint32_t fileId);
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
