#ifndef Rdm_h
#define Rdm_h

#include <QByteArray>
#include <QIODevice>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include "Server.h"
#include <RTags.h>
#include "Location.h"
#include "ResponseMessage.h"

class CursorInfo;
class CXStringScope
{
public:
    CXStringScope(CXString str)
        : string(str)
    {}

    ~CXStringScope()
    {
        clang_disposeString(string);
    }
    CXString string;
};

struct FileInformation {
    FileInformation(time_t lt = 0, const QList<QByteArray> &args = QList<QByteArray>())
        : lastTouched(lt), compileArgs(args)
    {}

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
enum { DatabaseVersion = 9 };

enum ReferenceType {
    NormalReference,
    MemberFunction,
    GlobalFunction
};
}

class Database;
typedef QHash<Location, CursorInfo> SymbolHash;
typedef QHash<Location, QPair<Location, Rdm::ReferenceType> > ReferenceHash;
typedef QHash<QByteArray, QSet<Location> > SymbolNameHash;
typedef QHash<quint32, QSet<quint32> > DependencyHash;
typedef QPair<QByteArray, quint64> WatchedPair;
typedef QHash<QByteArray, Location> PchUSRHash;
typedef QHash<Path, QSet<WatchedPair> > WatchedHash;
typedef QHash<quint32, FileInformation> InformationHash;

namespace Rdm {
static inline bool isPch(const QList<QByteArray> &args)
{
    const int size = args.size();
    bool nextIsX = false;
    for (int i=0; i<size; ++i) {
        const QByteArray &arg = args.at(i);
        if (nextIsX) {
            return (arg == "c++-header" || arg == "c-header");
        } else if (arg == "-x") {
            nextIsX = true;
        } else if (arg.startsWith("-x")) {
            const QByteArray rest = QByteArray::fromRawData(arg.constData() + 2, arg.size() - 2);
            return (rest == "c++-header" || rest == "c-header");
        }
    }
    return false;
}

void setMaxMemoryUsage(quint64 max);
bool waitForMemory(int maxMs);
QByteArray eatString(CXString str);
QByteArray cursorToString(CXCursor cursor);
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

CursorInfo findCursorInfo(Database *db, const Location &key, Location *loc = 0);
int writeSymbolNames(SymbolNameHash &symbolNames);
int writeDependencies(const DependencyHash &dependencies);
int writePchDepencies(const QHash<Path, QSet<quint32> > &pchDependencies);
int writeFileInformation(quint32 fileId, const QList<QByteArray> &args, time_t lastTouched);
int writePchUSRHashes(const QHash<Path, PchUSRHash> &hashes);
int writeSymbols(SymbolHash &symbols, const ReferenceHash &references);
int dirty(const QSet<quint32> &dirtyFileIds);
QList<QByteArray> compileArgs(quint32 fileId);
// the symbols will be modified before writing and we don't want to detach so we
// work on a non-const reference
class LogObject : public QObject, public Output
{
    Q_OBJECT
public:
    LogObject(Connection *conn, int level)
        : QObject(conn), Output(level), mConnection(conn)
    {
        connect(conn, SIGNAL(disconnected()), conn, SLOT(deleteLater()));
    }

    virtual void log(const char *msg, int len)
    {
        const QByteArray out(msg, len);
        QMetaObject::invokeMethod(this, "onLog", Qt::QueuedConnection, Q_ARG(QByteArray, out));
    }
public slots:
    void onLog(const QByteArray &log)
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
