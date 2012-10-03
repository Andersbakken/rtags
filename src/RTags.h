#ifndef RTags_h
#define RTags_h

#include "ByteArray.h"
#include "Location.h"
#include "Log.h"
#include "Path.h"
#include "SourceInformation.h"
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
    NoReference,
    NormalReference,
    LinkedReference
};

enum UnitType {
    CompileC,
    CompileCPlusPlus
};
enum CursorType {
    Include,
    Cursor,
    Reference,
    Other
};
}

class CursorInfo;
typedef Map<Location, CursorInfo> SymbolMap;
typedef Map<Location, Map<Location, RTags::ReferenceType> > ReferenceMap;
typedef Map<ByteArray, Set<Location> > SymbolNameMap;
typedef Map<uint32_t, Set<uint32_t> > DependencyMap;
typedef Map<uint32_t, SourceInformation> SourceInformationMap;
typedef Map<Path, Set<ByteArray> > FilesMap;
typedef Map<Location, std::pair<int, ByteArray> > FixitMap;
typedef Map<uint32_t, List<ByteArray> > DiagnosticsMap;
typedef Map<Path, shared_ptr<Project> > ProjectsMap;
typedef Map<uint32_t, time_t> GRFilesMap;
// file id to last modified, time_t means currently parsing
typedef Map<ByteArray, Map<Location, bool> > GRMap;
// symbolName to Map<location, bool> bool == false means cursor, true means reference

namespace RTags {
void dirtySymbolNames(SymbolNameMap &map, const Set<uint32_t> &dirty);
void dirtySymbols(SymbolMap &map, const Set<uint32_t> &dirty);

ByteArray backtrace(int maxFrames = -1);


template <typename Container, typename Value>
inline bool addTo(Container &container, const Value &value)
{
    const int oldSize = container.size();
    container += value;
    return container.size() != oldSize;
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

inline bool match(uint32_t fileId, const Location &loc)
{
    return loc.fileId() == fileId;
}

inline bool match(const Set<uint32_t> &fileIds, const Location &loc)
{
    return fileIds.contains(loc.fileId());
}

#endif
