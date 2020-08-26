/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef RTags_h
#define RTags_h

#if defined(__clang__)
#if (__clang_major__ * 10000 + __clang_minor__ * 100 + __clang_patchlevel__) >= 30400
#define HAS_JSON_H
#endif
#elif defined(__GNUC__)
#if (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__) >= 40900
#define HAS_JSON_H
#endif
#else // other compiler, assume json.h is supported
#define HAS_JSON_H
#endif

#include <assert.h>
#include <ctype.h>
#include <unistd.h>
#include <clang-c/Index.h>
#include <stdint.h>
#include <string.h>
#include <typeinfo>
#include <utility>
#include <initializer_list>
#include <algorithm>
#include <functional>
#include <iosfwd>
#include <memory>
#include <string>
#include <system_error>
#include <vector>

#include "rct/rct-config.h"
#include "FixIt.h"
#include "Location.h"
#include "Source.h"
#include "Symbol.h"
#include "rct/Value.h"
#include "rct/Flags.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Set.h"
#include "rct/String.h"
#include "IndexMessage.h"
#include "Sandbox.h"
#include "clang-c/CXString.h"
#include "rct/Hash.h"
#include "rct/List.h"
#include "rct/Map.h"

class Database;
class Project;
struct Diagnostic;
struct DependencyNode;
class IndexDataMessage;

typedef List<std::pair<uint32_t, uint32_t> > Includes;
typedef Hash<uint32_t, DependencyNode*> Dependencies;
typedef Hash<uint32_t, SourceList> Sources;
typedef Map<Path, Set<String> > Files;
typedef Hash<uint32_t, Set<FixIt> > FixIts;
typedef Hash<Path, String> UnsavedFiles;

struct SourceCache;

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

inline bool operator==(const CXCursor &l, const CXCursor &r)
{
    return clang_equalCursors(l, r);
}
inline bool operator!=(const CXCursor &l, const CXCursor &r)
{
    return !clang_equalCursors(l, r);
}

inline bool operator!(const CXCursor &cursor)
{
    return clang_Cursor_isNull(cursor);
}

inline bool operator!(const CXSourceLocation &location)
{
    static const CXSourceLocation sNullLocation = clang_getNullLocation();
    return clang_equalLocations(location, sNullLocation);
}

namespace RTags {

String versionString();

const LogLevel DiagnosticsLevel(-2);
const LogLevel Statistics(-3);

enum ExitCode {
    Success = 0,
    GeneralFailure = 32,
    NetworkFailure = 33,
    TimeoutFailure = 34,
    NotIndexed = 35,
    ConnectionFailure = 36,
    ProtocolFailure = 37,
    ArgumentParseError = 38,
    UnexpectedMessageError = 39,
    UnknownMessageError = 40
};
enum UnitType {
    CompileC,
    CompileCPlusPlus
};
enum CursorType {
    Type_Include,
    Type_Cursor,
    Type_Reference,
    Type_Statement,
    Type_Literal,
    Type_Other
};
void initMessages();

String eatString(CXString str);
enum CursorToStringFlags {
    NoCursorToStringFlags = 0x00,
    IncludeUSR = 0x01,
    IncludeRange = 0x02,
    DefaultCursorToStringFlags = IncludeRange,
    IncludeSpecializedUsr = 0x04,
    IncludeStructSizeof = 0x08,
    RealPathCursorPath = 0x10,
    AllCursorToStringFlags = IncludeUSR|IncludeRange|IncludeSpecializedUsr|IncludeStructSizeof|RealPathCursorPath
};
RCT_FLAGS(CursorToStringFlags);
String cursorToString(CXCursor cursor, Flags<CursorToStringFlags> = DefaultCursorToStringFlags);

RCT_FLAGS(CXTranslationUnit_Flags);

struct TranslationUnit {
    TranslationUnit()
        : index(nullptr), unit(nullptr)
    {}
    ~TranslationUnit()
    {
        clear();
    }
    static void visit(CXCursor c, std::function<CXChildVisitResult(CXCursor)> func)
    {
        clang_visitChildren(c, [](CXCursor cursor, CXCursor, CXClientData data) {
                return (*reinterpret_cast<std::function<CXChildVisitResult(CXCursor)> *>(data))(cursor);
            }, &func);
    }
    void visit(std::function<CXChildVisitResult(CXCursor)> func)
    {
        visit(cursor(), func);
    }

    void clear()
    {
        if (unit) {
            clang_disposeTranslationUnit(unit);
            unit = nullptr;
        }
        if (index) {
            clang_disposeIndex(index);
            index = nullptr;
        }
    }

    CXCursor cursor() const { return clang_getTranslationUnitCursor(unit); }

    bool reparse(CXUnsavedFile *unsaved, int unsavedCount);
    static std::shared_ptr<TranslationUnit> create(const Path &sourceFile,
                                                   const List<String> &args,
                                                   CXUnsavedFile *unsaved,
                                                   int unsavedCount,
                                                   Flags<CXTranslationUnit_Flags> translationUnitFlags = CXTranslationUnit_None,
                                                   bool displayDiagnostics = true);

    static std::shared_ptr<TranslationUnit> load(const Path &path);

    CXIndex index;
    CXTranslationUnit unit;
    String clangLine;
};

struct CreateLocation
{
    virtual ~CreateLocation() {}
    inline Location createLocation(const CXSourceLocation &location, bool *blocked = nullptr, unsigned *offset = nullptr)
    {
        CXString fileName;
        unsigned int line, col;
        CXFile file;
        clang_getSpellingLocation(location, &file, &line, &col, offset);
        if (file) {
            fileName = clang_getFileName(file);
        } else {
            if (blocked)
                *blocked = false;
            return Location();
        }
        const char *fn = clang_getCString(fileName);
        assert(fn);
        if (!*fn || !strcmp("<built-in>", fn) || !strcmp("<command line>", fn)) {
            if (blocked)
                *blocked = false;
            clang_disposeString(fileName);
            return Location();
        }
        const Path path = RTags::eatString(fileName);
        const Location ret = createLocation(path, line, col, blocked);
        return ret;
    }
    inline Location createLocation(CXFile file, unsigned int line, unsigned int col, bool *blocked = nullptr)
    {
        if (blocked)
            *blocked = false;
        if (!file)
            return Location();

        CXString fn = clang_getFileName(file);
        const char *cstr = clang_getCString(fn);
        if (!cstr) {
            clang_disposeString(fn);
            return Location();
        }
        const Path p = Path::resolved(cstr);
        clang_disposeString(fn);
        return createLocation(p, line, col, blocked);
    }
    inline Location createLocation(const CXCursor &cursor, bool *blocked = nullptr, unsigned *offset = nullptr)
    {
        const CXSourceLocation location = clang_getCursorLocation(cursor);
        if (!location)
            return Location();
        return createLocation(location, blocked, offset);
    }

    virtual Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = nullptr) = 0;
};

struct DiagnosticsProvider
{
    virtual ~DiagnosticsProvider() {}

    inline CXFile getFile(size_t idx, const Path &path) const
    {
        return clang_getFile(unit(idx), path.constData());
    }

    inline CXCursor cursorAt(size_t idx, const CXSourceLocation &location) const
    {
        return clang_getCursor(unit(idx), location);
    }

    inline Location createLocation(const CXSourceLocation &location, bool *blocked = nullptr, unsigned *offset = nullptr)
    {
        CXString fileName;
        unsigned int line, col;
        CXFile file;
        clang_getSpellingLocation(location, &file, &line, &col, offset);
        if (file) {
            fileName = clang_getFileName(file);
        } else {
            if (blocked)
                *blocked = false;
            return Location();
        }
        const char *fn = clang_getCString(fileName);
        assert(fn);
        if (!*fn || !strcmp("<built-in>", fn) || !strcmp("<command line>", fn)) {
            if (blocked)
                *blocked = false;
            clang_disposeString(fileName);
            return Location();
        }
        const Path path = RTags::eatString(fileName);
        const Location ret = createLocation(path, line, col, blocked);
        return ret;
    }
    inline Location createLocation(CXFile file, unsigned int line, unsigned int col, bool *blocked = nullptr)
    {
        if (blocked)
            *blocked = false;
        if (!file)
            return Location();

        CXString fn = clang_getFileName(file);
        const char *cstr = clang_getCString(fn);
        if (!cstr) {
            clang_disposeString(fn);
            return Location();
        }
        bool ok;
        Path p = Path::resolved(cstr, Path::RealPath, Path(), &ok);
        clang_disposeString(fn);
        if (!ok) {
            p.canonicalize();
        }
        return createLocation(p, line, col, blocked);
    }
    Location createLocation(const CXCursor &cursor, CXCursorKind kind = CXCursor_FirstInvalid, bool *blocked = nullptr, unsigned *offset = nullptr);
    virtual size_t unitCount() const = 0;
    virtual size_t diagnosticCount(size_t unit) const = 0;
    virtual CXDiagnostic diagnostic(size_t unit, size_t idx) const = 0;
    virtual Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = nullptr) = 0;
    virtual uint32_t sourceFileId() const = 0;
    virtual IndexDataMessage &indexDataMessage() = 0;
    virtual CXTranslationUnit unit(size_t unit) const = 0;

    void diagnose();
};

struct Auto {
    CXCursor cursor;
    CXType type;
};
bool resolveAuto(const CXCursor &cursor, Auto *a = nullptr);

int cursorArguments(const CXCursor &cursor, List<CXCursor> *args = nullptr);

String usr(const CXCursor &cursor);

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
            return RTags::getArguments(cursor) == argumentCount;
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
CXCursor findChild(CXCursor parent, CXCursorKind kind, CXChildVisitResult mode = CXChildVisit_Recurse);
CXCursor findChild(CXCursor parent, const String &name, CXChildVisitResult mode = CXChildVisit_Recurse);
List<CXCursor> findChain(CXCursor parent, const List<CXCursorKind> &kinds);
List<CXCursor> children(CXCursor parent, const Filter &in = Filter(), const Filter &out = Filter());

inline const char *tokenKindSpelling(CXTokenKind kind)
{
    switch (kind) {
    case CXToken_Punctuation: return "Punctuation";
    case CXToken_Keyword: return "Keyword";
    case CXToken_Identifier: return "Identifier";
    case CXToken_Literal: return "Literal";
    case CXToken_Comment: return "Comment";
    }
    return nullptr;
}

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
    case CXCursor_LambdaExpr:
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

inline CursorType cursorType(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_IntegerLiteral:
    case CXCursor_FloatingLiteral:
    case CXCursor_ImaginaryLiteral:
    case CXCursor_StringLiteral:
    case CXCursor_CharacterLiteral:
    case CXCursor_CompoundLiteralExpr:
    case CXCursor_CXXBoolLiteralExpr:
    case CXCursor_CXXNullPtrLiteralExpr:
    case CXCursor_ObjCStringLiteral:
    case CXCursor_ObjCBoolLiteralExpr:
        return Type_Literal;
    case CXCursor_InclusionDirective:
        return Type_Include;
    case CXCursor_LambdaExpr:
        return Type_Cursor;
    case CXCursor_BreakStmt:
    case CXCursor_ContinueStmt:
    case CXCursor_SwitchStmt:
    case CXCursor_ForStmt:
    case CXCursor_WhileStmt:
    case CXCursor_DoStmt:
    case CXCursor_CompoundStmt:
    case CXCursor_ReturnStmt:
        return Type_Statement;
    default:
        if (clang_isStatement(static_cast<CXCursorKind>(kind))) {
            return Type_Other;
        } else if (RTags::isCursor(kind)) {
            return Type_Cursor;
        } else if (RTags::isReference(kind)) {
            return Type_Reference;
        }
    }
    return Type_Other;
}

inline bool isContainer(CXCursorKind kind)
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
    case CXCursor_LambdaExpr:
        return true;
    default:
        break;
    }
    return false;
}

inline bool needsQualifiers(CXCursorKind kind)
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

inline bool isNumber(CXTypeKind kind)
{
    switch (kind) {
    case CXType_Bool:
    case CXType_Char_U:
    case CXType_UChar:
    case CXType_Char16:
    case CXType_Char32:
    case CXType_UShort:
    case CXType_UInt:
    case CXType_ULong:
    case CXType_ULongLong:
    case CXType_UInt128:
    case CXType_Char_S:
    case CXType_SChar:
    case CXType_WChar:
    case CXType_Short:
    case CXType_Int:
    case CXType_Long:
    case CXType_LongLong:
    case CXType_Int128:
    case CXType_Float:
    case CXType_Double:
    case CXType_LongDouble:
        return true;
    default:
        break;
    }
    return false;
}

String typeName(const CXCursor &cursor);
inline const char *builtinTypeName(CXTypeKind kind)
{
    const char *ret = nullptr;
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
    default: break;
    }
    return ret;
}

inline const char *completionChunkKindSpelling(CXCompletionChunkKind kind)
{
    switch (kind) {
    case CXCompletionChunk_Optional: return "Optional";
    case CXCompletionChunk_TypedText: return "TypedText";
    case CXCompletionChunk_Text: return "Text";
    case CXCompletionChunk_Placeholder: return "Placeholder";
    case CXCompletionChunk_Informative: return "Informative";
    case CXCompletionChunk_CurrentParameter: return "CurrentParameter";
    case CXCompletionChunk_LeftParen: return "LeftParen";
    case CXCompletionChunk_RightParen: return "RightParen";
    case CXCompletionChunk_LeftBracket: return "LeftBracket";
    case CXCompletionChunk_RightBracket: return "RightBracket";
    case CXCompletionChunk_LeftBrace: return "LeftBrace";
    case CXCompletionChunk_RightBrace: return "RightBrace";
    case CXCompletionChunk_LeftAngle: return "LeftAngle";
    case CXCompletionChunk_RightAngle: return "RightAngle";
    case CXCompletionChunk_Comma: return "Comma";
    case CXCompletionChunk_ResultType: return "ResultType";
    case CXCompletionChunk_Colon: return "Colon";
    case CXCompletionChunk_SemiColon: return "SemiColon";
    case CXCompletionChunk_Equal: return "Equal";
    case CXCompletionChunk_HorizontalSpace: return "HorizontalSpace";
    case CXCompletionChunk_VerticalSpace: return "VerticalSpace";
    }
    return nullptr;
}

String typeString(const CXType &type);

struct SortedSymbol
{
    SortedSymbol(Location loc = Location(),
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
String encodeUrlComponent(const String &string);
String decodeUrlComponent(const String &string);

template <typename Container, typename Value>
inline bool addTo(Container &container, const Value &value)
{
    const int oldSize = container.size();
    container += value;
    return container.size() != oldSize;
}

inline bool isSymbol(char ch)
{
    return (isalnum(ch) || ch == '_' || ch == '~');
}

inline bool isFunctionVariable(const String &entry)
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

inline bool isOperator(char ch)
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

void encodePath(Path &path);
void decodePath(Path &path);
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
size_t findOffset(int line, int col, const String &contents, size_t offset = 0);
Path findProjectRoot(const Path &path, ProjectRootMode mode, SourceCache *cache = nullptr);
enum FindAncestorFlag {
    Shallow = 0x1,
    Wildcard = 0x2,
    Authoritative = 0x4
};
RCT_FLAGS(FindAncestorFlag);
Path findAncestor(const Path& path, const String &fn, Flags<FindAncestorFlag> flags, SourceCache *cache = nullptr);
Map<String, String> rtagsConfig(const Path &path, SourceCache *cache = nullptr);

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
        return 3;
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_ClassTemplate:
        return 0;
    case CXCursor_FieldDecl:
    case CXCursor_VarDecl:
    case CXCursor_ParmDecl:
        return 6;
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
        // functiondecl and cxx method must be more than cxx
        // CXCursor_FunctionTemplate. Since constructors for templatized
        // objects seem to come out as function templates
        return 4;
    case CXCursor_MacroDefinition:
        return 7;
    default:
        break;
    }
    return 2;
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

inline void sortTargets(List<Symbol> &targets)
{
    targets.sort([](const Symbol &l, const Symbol &r) {
            const int lrank = RTags::targetRank(l.kind);
            const int rrank = RTags::targetRank(r.kind);
            if (lrank != rrank)
                return lrank > rrank;
            if (l.isDefinition() != r.isDefinition())
                return l.isDefinition();
            return l.location < r.location;
        });
}

inline List<Symbol> sortTargets(Set<Symbol> &&set)
{
    List<Symbol> targets;
    targets.resize(set.size());
    size_t i=0;
    for (auto &sym : set) {
        targets[i++] = std::move(sym);
    }
    sortTargets(targets);
    return targets;
}

inline List<Symbol> sortTargets(const Set<Symbol> &set)
{
    return sortTargets(Set<Symbol>(set));
}

inline String xmlEscape(const String& xml)
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

inline const String elispEscape(const String &data)
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
String toElisp(const Value &value);

inline Location createLocation(CXSourceLocation loc, int *offsetPtr = nullptr)
{
    if (offsetPtr)
        *offsetPtr = -1;
    CXString fileName;
    unsigned int line, col, offset;
    CXFile file;
    clang_getSpellingLocation(loc, &file, &line, &col, &offset);
    if (file) {
        fileName = clang_getFileName(file);
    } else {
        return Location();
    }
    const char *fn = clang_getCString(fileName);
    assert(fn);
    if (!*fn || !strcmp("<built-in>", fn) || !strcmp("<command line>", fn)) {
        clang_disposeString(fileName);
        return Location();
    }
    Path path = RTags::eatString(fileName);
    uint32_t fileId = Location::fileId(path);
    if (!fileId) {
        path.resolve();
        fileId = Location::insertFile(path);
    }
    if (offsetPtr)
        *offsetPtr = offset;
    return Location(fileId, line, col);
}

inline Location createLocation(const CXCursor &cursor, int *offsetPtr = nullptr)
{
    return createLocation(clang_getCursorLocation(cursor), offsetPtr);
}

inline bool isValid(const CXCursor &cursor)
{
    return !!cursor;
}

inline bool isValid(CXCursorKind kind)
{
    return !clang_isInvalid(kind);
}
inline bool isValid(const CXSourceLocation &location)
{
    return !!location;
}
}

namespace std
{
template <> struct hash<CXCursor> : public unary_function<CXCursor, size_t>
{
    size_t operator()(const CXCursor &value) const
    {
        return clang_hashCursor(value);
    }
};
}

template <typename Container>
inline String dumpFileIds(const Container &container)
{
    String ret;
    {
        Log l(&ret);
        for (uint32_t fileId : container) {
            l << Location::path(fileId);
        }
    }
    return ret;
}

struct SourceCache
{
    Hash<Path, Map<String, String> > rtagsConfigCache;
    Hash<Path, std::pair<Path, bool> > compilerCache; // bool signifies isCompiler, not just executable
    struct AncestorCacheKey {
        String string;
        Flags<RTags::FindAncestorFlag> flags;

        bool operator<(const AncestorCacheKey &other) const
        {
            const int cmp = string.compare(other.string);
            if (cmp)
                return cmp < 0;
            return flags < other.flags;
        }
    };
    Hash<Path, Map<AncestorCacheKey, Path> > ancestorCache;
};

inline Log operator<<(Log dbg, CXCursor cursor);
inline Log operator<<(Log dbg, CXType type);
inline Log operator<<(Log dbg, CXCursorKind kind);
inline Log operator<<(Log dbg, CXTypeKind kind);
inline Log operator<<(Log dbg, CXLinkageKind kind);
inline Log operator<<(Log dbg, CXCompletionChunkKind kind);

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
    bool operator==(const std::string &str) const { return !strcmp(clang_getCString(string), str.c_str()); }
    bool operator!=(const std::string &str) const { return strcmp(clang_getCString(string), str.c_str()); }
private:
    CXString string;
};

inline Log operator<<(Log dbg, CXCursor cursor)
{
    dbg << RTags::cursorToString(cursor, RTags::AllCursorToStringFlags);
    return dbg;
}

inline Log operator<<(Log dbg, CXType type)
{
    dbg << type.kind << RTags::eatString(clang_getTypeSpelling(type));
    return dbg;
}

inline Log operator<<(Log dbg, CXCursorKind kind)
{
    dbg << RTags::eatString(clang_getCursorKindSpelling(kind));
    return dbg;
}

inline Log operator<<(Log dbg, CXTypeKind kind)
{
    dbg << static_cast<int>(kind) << RTags::eatString(clang_getTypeKindSpelling(kind));
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

inline Log operator<<(Log dbg, CXCompletionChunkKind kind)
{
    switch (kind) {
    case CXCompletionChunk_Optional: dbg << "CXCompletionChunk_Optional"; break;
    case CXCompletionChunk_TypedText: dbg << "CXCompletionChunk_TypedText"; break;
    case CXCompletionChunk_Text: dbg << "CXCompletionChunk_Text"; break;
    case CXCompletionChunk_Placeholder: dbg << "CXCompletionChunk_Placeholder"; break;
    case CXCompletionChunk_Informative: dbg << "CXCompletionChunk_Informative"; break;
    case CXCompletionChunk_CurrentParameter: dbg << "CXCompletionChunk_CurrentParameter"; break;
    case CXCompletionChunk_LeftParen: dbg << "CXCompletionChunk_LeftParen"; break;
    case CXCompletionChunk_RightParen: dbg << "CXCompletionChunk_RightParen"; break;
    case CXCompletionChunk_LeftBracket: dbg << "CXCompletionChunk_LeftBracket"; break;
    case CXCompletionChunk_RightBracket: dbg << "CXCompletionChunk_RightBracket"; break;
    case CXCompletionChunk_LeftBrace: dbg << "CXCompletionChunk_LeftBrace"; break;
    case CXCompletionChunk_RightBrace: dbg << "CXCompletionChunk_RightBrace"; break;
    case CXCompletionChunk_LeftAngle: dbg << "CXCompletionChunk_LeftAngle"; break;
    case CXCompletionChunk_RightAngle: dbg << "CXCompletionChunk_RightAngle"; break;
    case CXCompletionChunk_Comma: dbg << "CXCompletionChunk_Comma"; break;
    case CXCompletionChunk_ResultType: dbg << "CXCompletionChunk_ResultType"; break;
    case CXCompletionChunk_Colon: dbg << "CXCompletionChunk_Colon"; break;
    case CXCompletionChunk_SemiColon: dbg << "CXCompletionChunk_SemiColon"; break;
    case CXCompletionChunk_Equal: dbg << "CXCompletionChunk_Equal"; break;
    case CXCompletionChunk_HorizontalSpace: dbg << "CXCompletionChunk_HorizontalSpace"; break;
    case CXCompletionChunk_VerticalSpace: dbg << "CXCompletionChunk_VerticalSpace"; break;
    }
    return dbg;
}

#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 29)
inline Log operator<<(Log dbg, CXTemplateArgumentKind kind)
{
    switch (kind) {
    case CXTemplateArgumentKind_Null: dbg << "Null"; break;
    case CXTemplateArgumentKind_Type: dbg << "Type"; break;
    case CXTemplateArgumentKind_Declaration: dbg << "Declaration"; break;
    case CXTemplateArgumentKind_NullPtr: dbg << "NullPtr"; break;
    case CXTemplateArgumentKind_Integral: dbg << "Integral"; break;
    case CXTemplateArgumentKind_Template: dbg << "Template"; break;
    case CXTemplateArgumentKind_TemplateExpansion: dbg << "TemplateExpansion"; break;
    case CXTemplateArgumentKind_Expression: dbg << "Expression"; break;
    case CXTemplateArgumentKind_Pack: dbg << "Pack"; break;
    case CXTemplateArgumentKind_Invalid: dbg << "Invalid"; break;
    }
    return dbg;
}
#endif

inline Log operator<<(Log dbg, CXAvailabilityKind kind)
{
    switch (kind) {
    case CXAvailability_Available: dbg << "Available"; break;
    case CXAvailability_Deprecated: dbg << "Deprecated"; break;
    case CXAvailability_NotAvailable: dbg << "NotAvailable"; break;
    case CXAvailability_NotAccessible: dbg << "NotAccessible"; break;
    }
    return dbg;
}

inline Log operator<<(Log dbg, CXLanguageKind kind)
{
    switch (kind) {
    case CXLanguage_Invalid: dbg << "Invalid"; break;
    case CXLanguage_C: dbg << "C"; break;
    case CXLanguage_ObjC: dbg << "ObjC"; break;
    case CXLanguage_CPlusPlus: dbg << "CPlusPlus"; break;
    }
    return dbg;
}

inline Log operator<<(Log dbg, CXDiagnosticSeverity severity)
{
    switch (severity) {
    case CXDiagnostic_Ignored: dbg << "Ignored"; break;
    case CXDiagnostic_Note: dbg << "Note"; break;
    case CXDiagnostic_Warning: dbg << "Warning"; break;
    case CXDiagnostic_Error: dbg << "Error"; break;
    case CXDiagnostic_Fatal: dbg << "Fatal"; break;
    }
    return dbg;
}

inline Log operator<<(Log dbg, CXCallingConv conv)
{
    switch (conv) {
    case CXCallingConv_Default: dbg << "Default"; break;
    case CXCallingConv_C: dbg << "C"; break;
    case CXCallingConv_X86StdCall: dbg << "X86StdCall"; break;
    case CXCallingConv_X86FastCall: dbg << "X86FastCall"; break;
    case CXCallingConv_X86ThisCall: dbg << "X86ThisCall"; break;
    case CXCallingConv_X86Pascal: dbg << "X86Pascal"; break;
    case CXCallingConv_AAPCS: dbg << "AAPCS"; break;
    case CXCallingConv_AAPCS_VFP: dbg << "AAPCS_VFP"; break;
    /* case CXCallingConv_PnaclCall: dbg << "PnaclCall"; break; */
    case CXCallingConv_IntelOclBicc: dbg << "IntelOclBicc"; break;
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
    case CXCallingConv_X86_64Win64: dbg << "X86_64Win64"; break;
    case CXCallingConv_X86_64SysV: dbg << "X86_64SysV"; break;
#endif
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 30)
    case CXCallingConv_X86VectorCall: dbg << "X86VectorCall"; break;
#endif
    case CXCallingConv_Invalid: dbg << "Invalid"; break;
    case CXCallingConv_Unexposed: dbg << "Unexposed"; break;
    default:
        break;
    }
    return dbg;
}

#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
inline Log operator<<(Log dbg, CXRefQualifierKind kind)
{
    switch (kind) {
    case CXRefQualifier_None: dbg << "None"; break;
    case CXRefQualifier_LValue: dbg << "lvalue"; break;
    case CXRefQualifier_RValue: dbg << "rvalue"; break;
    }
    return dbg;
}
#endif

inline Log operator<<(Log dbg, CXTypeLayoutError err)
{
    switch (err) {
    case CXTypeLayoutError_Invalid: dbg << "Invalid"; break;
    case CXTypeLayoutError_Incomplete: dbg << "Incomplete"; break;
    case CXTypeLayoutError_Dependent: dbg << "Dependent"; break;
    case CXTypeLayoutError_NotConstantSize: dbg << "NotConstantSize"; break;
    case CXTypeLayoutError_InvalidFieldName: dbg << "InvalidFieldName"; break;
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 53)
    case CXTypeLayoutError_Undeduced: dbg << "Undeduced"; break;
#endif
    }
    return dbg;
}

inline Log operator<<(Log dbg, CXTokenKind kind)
{
    dbg << RTags::tokenKindSpelling(kind);
    return dbg;
}

inline String &operator<<(String &str, CXString cxstr)
{
    if (const char *cstr = clang_getCString(cxstr))
        str.append(cstr);
    clang_disposeString(cxstr);
    return str;
}

#endif
