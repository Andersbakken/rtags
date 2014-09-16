/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef RTags_h
#define RTags_h

#include "rct-config.h"
#include <rct/String.h>
#include "Location.h"
#include <rct/Log.h>
#include "FixIt.h"
#include <rct/Path.h>
#include "Source.h"
#include <assert.h>
#include <getopt.h>
#include <stdio.h>
#include <typeinfo>

class Database;
class Project;
namespace RTags {

enum { DatabaseVersion = 49 };
enum { ASTManifestVersion = 1 };

enum {
    CompilationError = -1,
    CompilationErrorXml = -2,
    Statistics = -3
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
void initMessages();
}

class CursorInfo;
typedef Map<Location, std::shared_ptr<CursorInfo> > SymbolMap;
typedef Hash<uint32_t, SymbolMap> ErrorSymbolMap;
typedef Hash<String, Set<Location> > UsrMap;
typedef Map<Location, Set<Location> > ReferenceMap;
typedef Map<String, Set<Location> > SymbolNameMap;
typedef Hash<uint32_t, Set<uint32_t> > DependencyMap;
typedef Map<uint64_t, Source> SourceMap;
typedef Map<Path, Set<String> > FilesMap;
typedef Hash<uint32_t, Set<FixIt> > FixItMap;
typedef Hash<uint32_t, List<String> > DiagnosticsMap;
typedef Hash<Path, String> UnsavedFiles;

namespace RTags {
void dirtySymbolNames(SymbolNameMap &map, const Set<uint32_t> &dirty);
void dirtySymbols(SymbolMap &map, const Set<uint32_t> &dirty);
void dirtyUsr(UsrMap &map, const Set<uint32_t> &dirty);

template <typename Container, typename Value>
inline bool addTo(Container &container, const Value &value)
{
    const int oldSize = container.size();
    container += value;
    return container.size() != oldSize;
}

static inline bool isSymbol(char ch)
{
    return (isalnum(ch) || ch == '_' || ch == '~');
}

static inline bool isOperator(char ch)
{
    switch (ch) {
    case '!':
    case '%':
    case '&':
    case '(':
    case ')':
    case '+':
    case ',':
    case '-':
    case '.':
    case '/':
    case ':':
    case '<':
    case '=':
    case '>':
    case '?':
    case '[':
    case ']':
    case '^':
    case '|':
    case '~':
        return true;
    default:
        break;
    }
    return false;
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

#define DEFAULT_RDM_TCP_PORT 12526 // ( 100 'r' + (114 'd' * 109 'm')

inline std::pair<String, uint16_t> parseHost(const char *arg)
{
    std::pair<String, uint16_t> host;
    const char *colon = strchr(arg, ':');
    if (colon) {
        host.first.assign(arg, colon - arg);
        host.second = atoi(colon + 1);
        if (!host.second)
            host = std::make_pair<String, uint16_t>(String(), 0);
    } else {
        host.first = arg;
        host.second = DEFAULT_RDM_TCP_PORT;
    }
    return host;
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

enum ProjectRootMode {
    SourceRoot,
    BuildRoot
};
Path findProjectRoot(const Path &path, ProjectRootMode mode);
enum FindAncestorFlag {
    Shallow = 0x1,
    Wildcard = 0x2
};
Path findAncestor(Path path, const char *fn, unsigned flags);
Map<String, String> rtagsConfig(const Path &path);
}

#endif
