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

using namespace std::tr1;

class Database;
class Project;
namespace RTags {

enum DatabaseLockType {
    Read = ReadWriteLock::Read,
    Write = ReadWriteLock::Write,
    Erase
};

enum ReferenceType {
    NormalReference,
    MemberFunction,
    GlobalFunction
};

enum UnitType {
    CompileC,
    CompileCPlusPlus
};
}

class CursorInfo;
typedef Map<Location, CursorInfo> SymbolMap;
typedef Map<Location, Map<Location, RTags::ReferenceType> > ReferenceMap;
typedef Map<ByteArray, Set<Location> > SymbolNameMap;
typedef Map<uint32_t, Set<uint32_t> > DependencyMap;
typedef Map<uint32_t, List<ByteArray> > CompileArgumentsMap;
typedef Map<Path, Map<ByteArray, time_t> > GRFilesMap;
typedef Map<Location, std::pair<int, ByteArray> > FixitMap;
typedef Map<uint32_t, List<ByteArray> > DiagnosticsMap;
typedef Map<ByteArray, Map<Location, bool> > GRMap;
// key: absolute path to directory, value: Map<fileName, last modified>,
// last modified is 0 for non-source files

namespace RTags {
void dirtySymbolNames(SymbolNameMap &map, const Set<uint32_t> &dirty);
void dirtySymbols(SymbolMap &map, const Set<uint32_t> &dirty);

ByteArray backtrace(int maxFrames = -1);

inline bool isReference(CXCursorKind kind)
{
    if (clang_isReference(kind))
        return true;
    switch (kind) {
    case CXCursor_DeclRefExpr:
    case CXCursor_UnexposedDecl:
    case CXCursor_MacroExpansion:
    case CXCursor_MemberRefExpr:
        return true;
    default:
        break;
    }
    return false;
}

inline bool isCursor(CXCursorKind kind)
{
    if (clang_isDeclaration(kind))
        return true;
    switch (kind) {
    case CXCursor_LabelStmt:
    case CXCursor_MacroDefinition:
        return true;
    default:
        break;
    }
    return false;
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

SymbolMap::const_iterator findCursorInfo(const SymbolMap &map, const Location &location);
inline CursorInfo findCursorInfo(const SymbolMap &map, const Location &location, Location *key)
{
    const SymbolMap::const_iterator it = findCursorInfo(map, location);
    if (it == map.end()) {
        if (key)
            key->clear();
        return CursorInfo();
    }
    if (key)
        *key = it->first;
    return it->second;
}

enum TimeFormat {
    DateTime,
    Time,
    Date
};
inline ByteArray timeToString(time_t t, TimeFormat fmt)
{
    const char *format = 0;
    switch (fmt) {
    case DateTime:
        format = "%Y-%m-%d %H:%M:%S";
        break;
    case Date:
        format = "%Y-%m-%d";
        break;
    case Time:
        format = "%H:%M:%S";
        break;
    }

    char buf[32];
    tm tm;
    localtime_r(&t, &tm);
    const int w = strftime(buf, sizeof(buf), format, &tm);
    return ByteArray(buf, w);
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

template <typename T> class Ptr : public shared_ptr<T>
{
public:
    Ptr(T *t = 0)
        : shared_ptr<T>(t)
    {}
    operator T*() const { return shared_ptr<T>::get(); }
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
    const char *data() const
    {
        return clang_getCString(string);
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

inline Log operator<<(Log dbg, CXCursor cursor)
{
    dbg << RTags::cursorToString(cursor);
    return dbg;
}

inline Log operator<<(Log dbg, CXCursorKind kind)
{
    dbg << RTags::eatString(clang_getCursorKindSpelling(kind));
    return dbg;
}

#endif
