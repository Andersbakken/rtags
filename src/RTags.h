/* This file is part of RTags (http://rtags.net).

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
#include "FixIt.h"
#include "Location.h"
#include "Source.h"
#include "Symbol.h"
#include <assert.h>
#include <getopt.h>
#include <rct/Log.h>
#include <rct/Path.h>
#include <rct/String.h>
#include <stdio.h>
#include <typeinfo>
#include <rct/Flags.h>

class Database;
class Project;
namespace RTags {

enum {
    MajorVersion = 2,
    MinorVersion = 0,
    DatabaseVersion = 67
};

inline String versionString()
{
    return String::format<64>("%d.%d.%d", MajorVersion, MinorVersion, DatabaseVersion);
}

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
    Type_Include,
    Type_Cursor,
    Type_Reference,
    Type_Other
};
void initMessages();
}

struct Diagnostic;
typedef Map<Location, Diagnostic> Diagnostics;
struct DependencyNode;
typedef List<std::pair<uint32_t, uint32_t> > Includes;
typedef Hash<uint32_t, DependencyNode*> Dependencies;
typedef Hash<String, Set<uint32_t> > Declarations;
typedef Map<uint64_t, Source> Sources;
typedef Map<Path, Set<String> > Files;
typedef Hash<uint32_t, Set<FixIt> > FixIts;
typedef Hash<Path, String> UnsavedFiles;

namespace RTags {
Path encodeSourceFilePath(const Path &dataDir, const Path &project, uint32_t fileId = 0);

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

static inline bool isFunctionVariable(const String &entry)
{
    assert(entry.contains('('));
    const int endParen = entry.lastIndexOf(')');
    if (endParen != -1) {
        const char *p = entry.constData() + endParen;
        if (*++p == ':' && *++p == ':') {
            while (*++p) {
                if (!RTags::isSymbol(*p))
                    return false;
            }
            return true;
        }
    }
    return false;
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
RCT_FLAGS(FindAncestorFlag);
Path findAncestor(Path path, const char *fn, Flags<FindAncestorFlag> flags);
Map<String, String> rtagsConfig(const Path &path);

enum { DefinitionBit = 0x1000 };
inline CXCursorKind targetsValueKind(uint16_t val)
{
    return static_cast<CXCursorKind>(val & ~DefinitionBit);
}
inline bool targetsValueIsDefinition(uint16_t val)
{
    return val & DefinitionBit;
}
inline uint16_t createTargetsValue(CXCursorKind kind, bool definition)
{
    return (kind | (definition ? DefinitionBit : 0));
}
inline uint16_t createTargetsValue(const CXCursor &cursor)
{
    return createTargetsValue(clang_getCursorKind(cursor), clang_isCursorDefinition(cursor));
}
inline int targetRank(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_Constructor: // this one should be more than class/struct decl
        return 1;
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_ClassTemplate:
        return 0;
    case CXCursor_FieldDecl:
    case CXCursor_VarDecl:
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
        // functiondecl and cxx method must be more than cxx
        // CXCursor_FunctionTemplate. Since constructors for templatatized
        // objects seem to come out as function templates
        return 3;
    case CXCursor_MacroDefinition:
        return 4;
    default:
        return 2;
    }
}
inline Symbol bestTarget(const Set<Symbol> &targets)
{
    Symbol ret;
    int bestRank = -1;
    for (auto t : targets) {
        const int rank = targetRank(t.kind);
        if (rank > bestRank || (rank == bestRank && t.isDefinition())) {
            ret = t;
            bestRank = rank;
        }
    }
    return ret;
}
}

#endif
