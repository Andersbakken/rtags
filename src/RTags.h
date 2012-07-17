#ifndef RTags_h
#define RTags_h

#include "ByteArray.h"
#include "CursorInfo.h"
#include "Location.h"
#include "Log.h"
#include "Path.h"
#include <assert.h>
#include <getopt.h>
#include <memory>
#include <stdio.h>
#include <typeinfo>

class Database;
class ScopedDB;
namespace RTags {

enum ReferenceType {
    NormalReference,
    MemberFunction,
    GlobalFunction
};

enum UnitType {
    CompileC,
    CompileCPlusPlus,
    PchC,
    PchCPlusPlus
};
}

class FileInformation;
class CursorInfo;
typedef Map<Location, CursorInfo> SymbolMap;
typedef Map<Location, std::pair<Location, RTags::ReferenceType> > ReferenceMap;
typedef Map<ByteArray, Set<Location> > SymbolNameMap;
typedef Map<uint32_t, Set<uint32_t> > DependencyMap;
typedef std::pair<ByteArray, time_t> WatchedPair;
typedef Map<ByteArray, Location> PchUSRMap;
typedef Map<Path, Set<WatchedPair> > WatchedMap;
typedef Map<uint32_t, FileInformation> InformationMap;

namespace RTags {
void dirtySymbolNames(ScopedDB &db, const Set<uint32_t> &dirty);
int dirtySymbols(ScopedDB &db, const Set<uint32_t> &dirty);

inline bool isPch(const List<ByteArray> &args)
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

ByteArray backtrace(int maxFrames = -1);

inline bool isReference(CXCursorKind kind)
{
    return (clang_isReference(kind) || (kind >= CXCursor_FirstExpr && kind <= CXCursor_LastExpr) || kind == CXCursor_UnexposedExpr); // ### not sure about this one
}

inline bool hasAdditionalReferences(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_Constructor:
    case CXCursor_Destructor:
        return true;
    default:
        return false;
    }
}

ByteArray eatString(CXString str);
ByteArray cursorToString(CXCursor cursor);
template <typename T>
inline bool startsWith(const List<T> &list, const T &str)
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
inline bool addTo(Container &container, const Value &value)
{
    const int oldSize = container.size();
    container += value;
    return container.size() != oldSize;
}

CursorInfo findCursorInfo(Database *db, const Location &key, Location *loc = 0);
List<ByteArray> compileArgs(uint32_t fileId, const Path &path);

inline ByteArray timeToString(time_t t)
{
    char buf[32];
    tm tm;
    localtime_r(&t, &tm);
    const int w = strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", &tm);
    return ByteArray(buf, w);
}

inline Path rtagsDir()
{
    char buf[128];
    int w = snprintf(buf, sizeof(buf), "%s/.rtags/", getenv("HOME"));
    return Path(buf, w);
}

inline bool encodePath(Path &path)
{
    int size = path.size();
    enum { EncodedUnderscoreLength = 12 };
    for (int i=0; i<size; ++i) {
        char &ch = path[i];
        switch (ch) {
        case '/':
            ch = '_';
            break;
        case '_':
            path.replace(i, 1, "<underscore>");
            size += EncodedUnderscoreLength - 1;
            i += EncodedUnderscoreLength - 1;
            break;
        case '<':
            if (i + EncodedUnderscoreLength <= size && !strncmp(&ch + 1, "underscore>", EncodedUnderscoreLength - 1)) {
                error("Invalid folder name %s", path.constData());
                return false;
            }
            break;
        }
    }
    return true;
}

inline void decodePath(Path &path)
{
    int size = path.size();
    enum { EncodedUnderscoreLength = 12 };
    for (int i=0; i<size; ++i) {
        char &ch = path[i];
        switch (ch) {
        case '_':
            ch = '/';
            break;
        case '<':
            if (i + EncodedUnderscoreLength <= size && !strncmp(&ch + 1, "underscore>", EncodedUnderscoreLength - 1)) {
                path.replace(i, EncodedUnderscoreLength, "_");
                size -= EncodedUnderscoreLength - 1;
            }
            break;
        }
    }
}


inline int digits(int len)
{
    int ret = 1;
    while (len >= 10) {
        len /= 10;
        ++ret;
    }
    return ret;
}

ByteArray shortOptions(const option *longOptions);
int readLine(FILE *f, char *buf = 0, int max = -1);
void removeDirectory(const Path &path);
int canonicalizePath(char *path, int len);
ByteArray unescape(ByteArray command);

template <typename T> class Ptr : public std::tr1::shared_ptr<T>
{
public:
    Ptr(T *t = 0)
        : std::tr1::shared_ptr<T>(t)
    {}
    operator T*() const { return std::tr1::shared_ptr<T>::get(); }
};
bool startProcess(const Path &dotexe, const List<ByteArray> &dollarArgs);

void findApplicationDirPath(const char *argv0);
Path applicationDirPath();
}

#define eintrwrap(VAR, BLOCK)                   \
    do {                                        \
        VAR = BLOCK;                            \
    } while (VAR == -1 && errno == EINTR)

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

inline bool match(uint32_t fileId, const Location &loc)
{
    return loc.fileId() == fileId;
}

inline bool match(const Set<uint32_t> &fileIds, const Location &loc)
{
    return fileIds.contains(loc.fileId());
}

inline std::ostringstream &operator<<(std::ostringstream &dbg, CXCursor cursor)
{
    dbg << RTags::cursorToString(cursor);
    return dbg;
}

inline std::ostringstream &operator<<(std::ostringstream &dbg, CXCursorKind kind)
{
    dbg << RTags::eatString(clang_getCursorKindSpelling(kind));
    return dbg;
}

#endif
