#ifndef Rdm_h
#define Rdm_h

#include <ByteArray.h>
#include <Serializer.h>
#include <clang-c/Index.h>
#include <Path.h>
#include <RTags.h>
#include "Mutex.h"
#include "MutexLocker.h"
#include "Location.h"
#include "Log.h"
#include "ResponseMessage.h"
#include "CursorInfo.h"

struct FileInformation {
    FileInformation(time_t lt = 0, const List<ByteArray> &args = List<ByteArray>())
        : lastTouched(lt), compileArgs(args)
    {}

    time_t lastTouched;
    List<ByteArray> compileArgs;
};

static inline Serializer &operator<<(Serializer &s, const FileInformation &ci)
{
    s << ci.lastTouched << ci.compileArgs;
    return s;
}

static inline Deserializer &operator>>(Deserializer &ds, FileInformation &ci)
{
    ds >> ci.lastTouched >> ci.compileArgs;
    return ds;
}

struct MakefileInformation {
    MakefileInformation(time_t lt = 0,
                        const List<ByteArray> &args = List<ByteArray>(),
                        const List<ByteArray> &flags = List<ByteArray>())
        : lastTouched(lt), makefileArgs(args), extraFlags(flags)
    {}
    time_t lastTouched;
    List<ByteArray> makefileArgs;
    List<ByteArray> extraFlags;
};

static inline Serializer &operator<<(Serializer &s, const MakefileInformation &mi)
{
    s << mi.lastTouched << mi.makefileArgs << mi.extraFlags;
    return s;
}

static inline Deserializer &operator>>(Deserializer &s, MakefileInformation &mi)
{
    s >> mi.lastTouched >> mi.makefileArgs >> mi.extraFlags;
    return s;
}

namespace Rdm {
enum { DatabaseVersion = 13 };

enum ReferenceType {
    NormalReference,
    MemberFunction,
    GlobalFunction
};
}

class Database;
typedef Map<Location, CursorInfo> SymbolMap;
typedef Map<Location, std::pair<Location, Rdm::ReferenceType> > ReferenceMap;
typedef Map<ByteArray, Set<Location> > SymbolNameMap;
typedef Map<uint32_t, Set<uint32_t> > DependencyMap;
typedef std::pair<ByteArray, time_t> WatchedPair;
typedef Map<ByteArray, Location> PchUSRMap;
typedef Map<Path, Set<WatchedPair> > WatchedMap;
typedef Map<uint32_t, FileInformation> InformationMap;

namespace Rdm {
static inline ByteArray timeToString(time_t t)
{
    char buf[32];
    tm tm;
    localtime_r(&t, &tm);
    const int w = strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", &tm);
    return ByteArray(buf, w);
}

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

ByteArray eatString(CXString str);
ByteArray cursorToString(CXCursor cursor);
template <typename T>
static inline bool startsWith(const List<T> &list, const T &str)
{
    if (!list.isEmpty()) {
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
int writePchUSRMaps(const Map<Path, PchUSRMap> &maps);
int writeSymbols(SymbolMap &symbols, const ReferenceMap &references);

List<ByteArray> compileArgs(uint32_t fileId);
// the symbols will be modified before writing and we don't want to detach so we
// work on a non-const reference
}

static inline std::ostringstream &operator<<(std::ostringstream &dbg, CXCursor cursor)
{
    dbg << Rdm::cursorToString(cursor);
    return dbg;
}

static inline std::ostringstream &operator<<(std::ostringstream &dbg, CXCursorKind kind)
{
    dbg << Rdm::eatString(clang_getCursorKindSpelling(kind));
    return dbg;
}


#endif
