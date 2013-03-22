#ifndef __RTAGSCLANG_H__
#define __RTAGSCLANG_H__

#include "Str.h"
#include "RTags.h"
#include "CursorInfo.h"

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

namespace RTags {

String eatString(CXString str);
enum CursorToStringFlags {
    NoCursorToStringFlags = 0x0,
    IncludeUSR = 0x1,
    IncludeRange = 0x2,
    DefaultCursorToStringFlags = IncludeRange,
    AllCursorToStringFlags = IncludeUSR|IncludeRange
};
String cursorToString(CXCursor cursor, unsigned = DefaultCursorToStringFlags);
SymbolMap::const_iterator findCursorInfo(const SymbolMap &map, const Location &location, bool *moved);
inline CursorInfo findCursorInfo(const SymbolMap &map, const Location &location, bool *moved, Location *key)
{
    const SymbolMap::const_iterator it = findCursorInfo(map, location, moved);
    if (it == map.end()) {
        if (key)
            key->clear();
        return CursorInfo();
    }
    if (key)
        *key = it->first;
    return it->second;
}

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
    if (clang_isReference(kind))
        return true;
    switch (kind) {
    case CXCursor_DeclRefExpr:
    case CXCursor_UnexposedDecl:
    case CXCursor_MacroExpansion:
    case CXCursor_MemberRefExpr:
    case CXCursor_CallExpr:
    case CXCursor_CXXDeleteExpr:
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
    return clang_isDeclaration(kind);
}

static inline CursorType cursorType(CXCursorKind kind)
{
    if (clang_isStatement(kind)) {
        return Other;
    } else if (RTags::isCursor(kind)) {
        return Cursor;
    } else if (RTags::isReference(kind)) {
        return Reference;
    } else if (kind == CXCursor_InclusionDirective) {
        return Include;
    }
    return Other;
}

static inline bool isContainer(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_CXXMethod:
    case CXCursor_Constructor:
    case CXCursor_FunctionDecl:
    case CXCursor_Destructor:
    case CXCursor_ClassTemplate:
    case CXCursor_Namespace:
    case CXCursor_ClassDecl:
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

struct SortedCursor
{
    SortedCursor(const Location &loc = Location(),
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

    bool operator<(const SortedCursor &other) const
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
    bool operator>(const SortedCursor &other) const
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
}

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
    dbg << RTags::cursorToString(cursor);
    return dbg;
}

inline Log operator<<(Log dbg, CXCursorKind kind)
{
    dbg << RTags::eatString(clang_getCursorKindSpelling(kind));
    return dbg;
}


#endif /* __RTAGSCLANG_H__ */
