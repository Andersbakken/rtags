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

#include <assert.h>
#include <ctype.h>
#include <getopt.h>
#include <typeinfo>
#include <utility>

#include <clang/Basic/Version.h>
#include <clang-c/Index.h>

#include "FixIt.h"
#include "Location.h"
#include "Source.h"
#include "Symbol.h"
#include "rct/Flags.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Set.h"
#include "rct/String.h"

class Database;
class Project;
struct Diagnostic;
struct DependencyNode;
typedef List<std::pair<uint32_t, uint32_t> > Includes;
typedef Hash<uint32_t, DependencyNode*> Dependencies;
typedef Map<uint64_t, Source> Sources;
typedef Map<Path, Set<String> > Files;
typedef Hash<uint32_t, Set<FixIt> > FixIts;
typedef Hash<Path, String> UnsavedFiles;

namespace RTags {

enum {
    MajorVersion = 2,
    MinorVersion = 0,
    DatabaseVersion = 83,
    SourcesFileVersion = 4
};

inline String versionString()
{
    return String::format<64>("%d.%d.%d", MajorVersion, MinorVersion, DatabaseVersion);
}

const LogLevel DiagnosticsLevel(-2);
const LogLevel Statistics(-3);

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

String eatString(CXString str);
enum CursorToStringFlags {
    NoCursorToStringFlags = 0x0,
    IncludeUSR = 0x1,
    IncludeRange = 0x2,
    DefaultCursorToStringFlags = IncludeRange,
    IncludeSpecializedUsr = 0x4,
    AllCursorToStringFlags = IncludeUSR|IncludeRange|IncludeSpecializedUsr
};
RCT_FLAGS(CursorToStringFlags);
String cursorToString(CXCursor cursor, Flags<CursorToStringFlags> = DefaultCursorToStringFlags);

RCT_FLAGS(CXTranslationUnit_Flags);

void parseTranslationUnit(const Path &sourceFile, const List<String> &args,
                          CXTranslationUnit &unit, CXIndex index,
                          CXUnsavedFile *unsaved, int unsavedCount,
                          Flags<CXTranslationUnit_Flags> translationUnitFlags = CXTranslationUnit_None,
                          String *clangLine = 0);
void reparseTranslationUnit(CXTranslationUnit &unit, CXUnsavedFile *unsaved, int unsavedCount);
struct Auto {
    CXCursor cursor;
    CXType type;
};
std::shared_ptr<Auto> resolveAuto(const CXCursor &cursor);

struct Filter
{
    enum Mode {
        And,
        Or
    };
    inline Filter(Mode m = Or)
        : argumentCount(-1), mode(m)
    {}

    inline bool isNull() const
    {
        return kinds.isEmpty() && names.isEmpty() && argumentCount == -1;
    }

    inline bool isValid() const
    {
        return !kinds.isEmpty() || !names.isEmpty() || argumentCount != -1;
    }

    inline bool match(const CXCursor &cursor) const
    {
        bool matched = false;
        if (!kinds.isEmpty()) {
            if (kinds.contains(clang_getCursorKind(cursor))) {
                if (mode == Or)
                    return true;
                matched = true;
            } else if (mode == And) {
                return false;
            }
        }
        if (!names.isEmpty()) {
            const String name = RTags::eatString(clang_getCursorSpelling(cursor));
            if (names.contains(name)) {
                if (mode == Or)
                    return true;
                matched = true;
            } else if (mode == And) {
                return false;
            }
        }
        if (argumentCount != -1) {
#if CLANG_VERSION_MINOR > 1
            return clang_Cursor_getNumArguments(cursor) == argumentCount;
#else
            return true;
#endif
        }
        return matched;
    }

    Set<CXCursorKind> kinds;
    Set<String> names;
    int argumentCount;
    Mode mode;
};

CXCursor findFirstChild(CXCursor parent);
CXCursor findChild(CXCursor parent, CXCursorKind kind);
CXCursor findChild(CXCursor parent, const String &name);
List<CXCursor> findChain(CXCursor parent, const List<CXCursorKind> &kinds);
List<CXCursor> children(CXCursor parent, const Filter &in = Filter(), const Filter &out = Filter());

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

inline bool isReference(CXCursorKind kind)
{
    if (clang_isReference(static_cast<CXCursorKind>(kind)))
        return true;
    switch (kind) {
    case CXCursor_DeclRefExpr:
    case CXCursor_UnexposedDecl:
    case CXCursor_MacroExpansion:
    case CXCursor_MemberRefExpr:
    case CXCursor_CallExpr:
    case CXCursor_ObjCMessageExpr:
    case CXCursor_CXXDeleteExpr:
        return true;
    default:
        break;
    }
    return false;
}

inline bool isFunction(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_FunctionTemplate:
    case CXCursor_FunctionDecl:
    case CXCursor_Constructor:
    case CXCursor_Destructor:
    case CXCursor_CXXMethod:
    case CXCursor_ObjCInstanceMethodDecl:
    case CXCursor_ObjCClassMethodDecl:
        return true;
    default:
        break;
    }
    return false;
}

inline bool isCursor(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_LabelStmt:
    case CXCursor_MacroDefinition:
    case CXCursor_FunctionTemplate:
        return true;
    case CXCursor_CXXAccessSpecifier:
        return false;
    default:
        break;
    }
    return clang_isDeclaration(static_cast<CXCursorKind>(kind));
}

static inline CursorType cursorType(CXCursorKind kind)
{
    if (kind == CXCursor_InclusionDirective)
        return Type_Include;
    if (clang_isStatement(static_cast<CXCursorKind>(kind))) {
        return Type_Other;
    } else if (RTags::isCursor(kind)) {
        return Type_Cursor;
    } else if (RTags::isReference(kind)) {
        return Type_Reference;
    }
    return Type_Other;
}

static inline bool isContainer(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_CXXMethod:
    case CXCursor_Constructor:
    case CXCursor_FunctionDecl:
    case CXCursor_FunctionTemplate:
    case CXCursor_Destructor:
    case CXCursor_ClassTemplate:
    case CXCursor_Namespace:
    case CXCursor_ClassDecl:
    case CXCursor_ObjCInterfaceDecl:
    case CXCursor_ObjCImplementationDecl:
    case CXCursor_ObjCInstanceMethodDecl:
    case CXCursor_ObjCClassMethodDecl:
    case CXCursor_StructDecl:
        return true;
    default:
        break;
    }
    return false;
}

static inline bool needsQualifiers(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_CXXMethod:
    case CXCursor_Constructor:
    case CXCursor_FunctionDecl:
    case CXCursor_Destructor:
    case CXCursor_VarDecl:
    case CXCursor_ParmDecl:
    case CXCursor_FieldDecl:
    case CXCursor_ClassTemplate:
    case CXCursor_Namespace:
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_EnumConstantDecl:
    case CXCursor_EnumDecl:
    case CXCursor_TypedefDecl:
        return true;
    default:
        break;
    }
    return false;
}

String typeName(const CXCursor &cursor);
static inline const char *builtinTypeName(CXTypeKind kind)
{
    const char *ret = 0;
    switch (kind) {
    case CXType_Void: ret = "void"; break;
    case CXType_Bool: ret = "bool"; break;
    case CXType_Char_U: ret = "unsigned char"; break;
    case CXType_UChar: ret = "unsigned char"; break;
    case CXType_Char16: ret = "char16"; break;
    case CXType_Char32: ret = "char32"; break;
    case CXType_UShort: ret = "unsigned short"; break;
    case CXType_UInt: ret = "unsigned int"; break;
    case CXType_ULong: ret = "unsigned long"; break;
    case CXType_ULongLong: ret = "unsigned long long"; break;
    case CXType_UInt128: ret = "uint128"; break;
    case CXType_Char_S: ret = "char"; break;
    case CXType_SChar: ret = "schar"; break;
    case CXType_WChar: ret = "wchar"; break;
    case CXType_Short: ret = "short"; break;
    case CXType_Int: ret = "int"; break;
    case CXType_Long: ret = "long"; break;
    case CXType_LongLong: ret = "long long"; break;
    case CXType_Int128: ret = "int128"; break;
    case CXType_Float: ret = "float"; break;
    case CXType_Double: ret = "double"; break;
    case CXType_LongDouble: ret = "long double"; break;
    default:
        break;
    }
    return ret;
}

String typeString(const CXType &type);

struct SortedSymbol
{
    SortedSymbol(const Location &loc = Location(),
                 bool definition = false,
                 CXCursorKind k = CXCursor_FirstInvalid)
        : location(loc), isDefinition(definition), kind(k)
    {}

    Location location;
    bool isDefinition;
    CXCursorKind kind;

    int rank() const
    {
        int val = 0;
        switch (kind) {
        case CXCursor_VarDecl:
        case CXCursor_FieldDecl:
            val = 2;
            break;
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
        case CXCursor_Constructor:
        case CXCursor_FunctionTemplate:
            val = 5;
            break;
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
        case CXCursor_ClassTemplate:
            val = 10;
            break;
        default:
            val = 1;
            break;
        }
        if (isDefinition)
            val += 100;
        return val;
    }

    bool operator<(const SortedSymbol &other) const
    {
        const int me = rank();
        const int him = other.rank();
        // error() << "comparing<" << location << "and" << other.location
        //         << me << him << isDefinition << other.isDefinition
        //         << RTags::eatString(clang_getCursorKindSpelling(kind))
        //         << RTags::eatString(clang_getCursorKindSpelling(other.kind));
        if (me != him)
            return me > him;
        return location < other.location;
    }
    bool operator>(const SortedSymbol &other) const
    {
        const int me = rank();
        const int him = other.rank();
        // error() << "comparing>" << location << "and" << other.location
        //         << me << him << isDefinition << other.isDefinition
        //         << RTags::eatString(clang_getCursorKindSpelling(kind))
        //         << RTags::eatString(clang_getCursorKindSpelling(other.kind));
        if (me != him)
            return me > him;
        return location > other.location;
    }
};

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

inline void encodePath(Path &path)
{
    int size = path.size();
    for (int i=0; i<size; ++i) {
        char &ch = path[i];
        switch (ch) {
        case '/':
            ch = '_';
            break;
        case '_':
            path.insert(++i, '_');
            ++size;
            break;
        }
    }
}

inline void decodePath(Path &path)
{
    int size = path.size();
    for (int i=0; i<size; ++i) {
        char &ch = path[i];
        if (ch == '_') {
            if (i + 1 < size && path.at(i + 1) == '_') {
                path.remove(i + 1, 1);
                --size;
            } else {
                ch = '/';
            }
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
static inline String xmlEscape(const String& xml)
{
    if (xml.isEmpty())
        return xml;

    String ret;
    ret.reserve(xml.size() * 1.1);
    const char* ch = xml.constData();
    while (*ch) {
        switch (*ch) {
        case '"':
            ret << "\\\"";
            break;
        case '<':
            ret << "&lt;";
            break;
        case '>':
            ret << "&gt;";
            break;
        case '&':
            ret << "&amp;";
            break;
        default:
            ret << *ch;
            break;
        }
        ++ch;
    }
    return ret;
}

static inline const String elispEscape(const String &data)
{
    String ret;
    const int size = data.size();
    const char *ch = data.constData();
    bool copied = false;
    for (int i=0; i<size; ++i) {
        switch (*ch) {
        case '"':
        case '\\':
        case '\n':
        case '\t':
            if (!copied) {
                copied = true;
                ret.reserve(size + 16);
                if (i)
                    ret.assign(data.constData(), i);
            }
            switch (*ch) {
            case '"':
                ret << "\\\"";
                break;
            case '\n':
                ret << "\\n";
                break;
            case '\t':
                ret << "\\t";
                break;
            case '\\':
                ret << "\\\\";
                break;
            }
            break;
        default:
            if (copied)
                ret << *ch;
            break;
        }
        ++ch;
    }
    if (!copied)
        return data;
    return ret;
}
}

inline bool operator==(const CXCursor &l, CXCursorKind r)
{
    return clang_getCursorKind(l) == r;
}
inline bool operator==(CXCursorKind l, const CXCursor &r)
{
    return l == clang_getCursorKind(r);
}

inline bool operator!=(const CXCursor &l, CXCursorKind r)
{
    return clang_getCursorKind(l) != r;
}
inline bool operator!=(CXCursorKind l, const CXCursor &r)
{
    return l != clang_getCursorKind(r);
}

inline Log operator<<(Log dbg, CXCursor cursor);
inline Log operator<<(Log dbg, CXCursorKind kind);
inline Log operator<<(Log dbg, CXTypeKind kind);
inline Log operator<<(Log dbg, CXLinkageKind kind);

static inline bool operator==(const CXCursor &l, const CXCursor &r) { return clang_equalCursors(l, r); }

class CXStringScope
{
public:
    CXStringScope(CXString str) : string(str)
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
    operator CXString () const { return string; }
private:
    CXString string;
};

inline Log operator<<(Log dbg, CXCursor cursor)
{
    dbg << RTags::cursorToString(cursor, RTags::AllCursorToStringFlags);
    return dbg;
}

inline Log operator<<(Log dbg, CXCursorKind kind)
{
    dbg << RTags::eatString(clang_getCursorKindSpelling(kind));
    return dbg;
}

inline Log operator<<(Log dbg, CXTypeKind kind)
{
    dbg << RTags::eatString(clang_getTypeKindSpelling(kind));
    return dbg;
}

inline Log operator<<(Log dbg, CXLinkageKind kind)
{
    switch (kind) {
    case CXLinkage_Invalid:
        dbg << "Invalid";
        break;
    case CXLinkage_NoLinkage:
        dbg << "NoLinkage";
        break;
    case CXLinkage_Internal:
        dbg << "Internal";
        break;
    case CXLinkage_UniqueExternal:
        dbg << "UniqueExternal";
        break;
    case CXLinkage_External:
        dbg << "External";
        break;
    }
    return dbg;
}

#endif
