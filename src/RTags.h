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
#include <unistd.h>

#include <clang/Basic/Version.h>
#include <clang-c/Index.h>

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

class Database;
class Project;
struct Diagnostic;
struct DependencyNode;
struct CompilationDataBaseInfo;
typedef List<std::pair<uint32_t, uint32_t> > Includes;
typedef Hash<uint32_t, DependencyNode*> Dependencies;
typedef Map<uint64_t, Source> Sources;
typedef Map<Path, Set<String> > Files;
typedef Hash<uint32_t, Set<FixIt> > FixIts;
typedef Hash<Path, String> UnsavedFiles;

namespace RTags {

String versionString();

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
    Type_Statement,
    Type_Literal,
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

struct TranslationUnit {
    TranslationUnit()
        : index(0), unit(0)
    {}
    ~TranslationUnit()
    {
        if (unit)
            clang_disposeTranslationUnit(unit);
        if (index)
            clang_disposeIndex(index);
    }
    static void visit(CXCursor cursor, std::function<CXChildVisitResult(CXCursor)> func)
    {
        clang_visitChildren(cursor, [](CXCursor cursor, CXCursor, CXClientData data) {
                return (*reinterpret_cast<std::function<CXChildVisitResult(CXCursor)> *>(data))(cursor);
            }, &func);
    }
    void visit(std::function<CXChildVisitResult(CXCursor)> func)
    {
        visit(cursor(), func);
    }

    CXCursor cursor() const { return clang_getTranslationUnitCursor(unit); }

    bool reparse(CXUnsavedFile *unsaved, int unsavedCount);
    static std::shared_ptr<TranslationUnit> create(const Path &sourceFile,
                                                   const List<String> &args,
                                                   CXUnsavedFile *unsaved,
                                                   int unsavedCount,
                                                   Flags<CXTranslationUnit_Flags> translationUnitFlags = CXTranslationUnit_None,
                                                   bool displayDiagnostics = true);

    CXIndex index;
    CXTranslationUnit unit;
    String clangLine;
};

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

inline const char *tokenKindSpelling(CXTokenKind kind)
{
    switch (kind) {
    case CXToken_Punctuation: return "Punctuation";
    case CXToken_Keyword: return "Keyword";
    case CXToken_Identifier: return "Identifier";
    case CXToken_Literal: return "Literal";
    case CXToken_Comment: return "Comment";
    }
    return 0;
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

String typeName(const CXCursor &cursor);
inline const char *builtinTypeName(CXTypeKind kind)
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
    return 0;
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
Path findProjectRoot(const Path &path, ProjectRootMode mode);
enum FindAncestorFlag {
    Shallow = 0x1,
    Wildcard = 0x2
};
RCT_FLAGS(FindAncestorFlag);
Path findAncestor(Path path, const char *fn, Flags<FindAncestorFlag> flags);
Map<String, String> rtagsConfig(const Path &path);
bool loadCompileCommands(const Hash<Path, CompilationDataBaseInfo> &infos, const Path &projectRoot);

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

inline Location createLocation(CXSourceLocation loc, int *offsetPtr = 0)
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

inline Location createLocation(const CXCursor &cursor, int *offsetPtr = 0)
{
    return createLocation(clang_getCursorLocation(cursor), offsetPtr);
}
}

namespace CommandLineParser {
template <typename T>
struct Option {
    const T option;
    const char *longOpt;
    const char shortOpt;
    const int argument;
    const String description;
};
enum ParseStatus {
    Parse_Exec,
    Parse_Ok,
    Parse_Error
};
enum Flag {
    NoFlag = 0x0,
    IgnoreUnknown = 0x1
};
RCT_FLAGS(Flag);
template <typename T>
ParseStatus parse(int &argc, char **argv, const Option<T> *opts, size_t count, Flags<Flag> flags, const std::function<ParseStatus(T)> &handler)
{
    optind = 1;
#ifdef OS_Darwin
    optreset = 1;
#endif
    opterr = (flags & IgnoreUnknown) ? 0 : 1;
    Hash<int, const Option<T> *> shortOptions, longOptions;
    List<option> options;
    String shortOptionsString;

    for (size_t i=0; i<count; ++i) {
        if (opts[i].option) {
            const option opt = { opts[i].longOpt, opts[i].argument, 0, opts[i].shortOpt };
            if (opts[i].shortOpt) {
                shortOptionsString.append(opts[i].shortOpt);
                switch (opts[i].argument) {
                case no_argument:
                    break;
                case required_argument:
                    shortOptionsString.append(':');
                    break;
                case optional_argument:
                    shortOptionsString.append("::");
                    break;
                }
                assert(!shortOptions.contains(opts[i].shortOpt));
                shortOptions[opts[i].shortOpt] = &opts[i];
            }
            if (opts[i].longOpt)
                longOptions[options.size()] = &opts[i];
            options.push_back(opt);
        }
    }

    if (getenv("RTAGS_DUMP_UNUSED")) {
        String unused;
        for (int i=0; i<26; ++i) {
            if (!shortOptionsString.contains('a' + i))
                unused.append('a' + i);
            if (!shortOptionsString.contains('A' + i))
                unused.append('A' + i);
        }
        printf("Unused: %s\n", unused.constData());
        for (size_t i=0; i<count; ++i) {
            if (opts[i].longOpt) {
                if (!opts[i].shortOpt) {
                    printf("No shortoption for %s\n", opts[i].longOpt);
                } else if (opts[i].longOpt[0] != opts[i].shortOpt) {
                    printf("Not ideal option for %s|%c\n", opts[i].longOpt, opts[i].shortOpt);
                }
            }
        }
        return Parse_Ok;
    }

    {
        const option opt = { 0, 0, 0, 0 };
        options.push_back(opt);
    }

    ParseStatus ret = Parse_Exec;
    while (ret == Parse_Exec) {
        int idx = -1;
        const int c = getopt_long(argc, argv, shortOptionsString.constData(), options.data(), &idx);
        switch (c) {
        case -1:
            return ret;
        case '?':
        case ':':
            if (!(flags & IgnoreUnknown))
                return Parse_Error;
            continue;
        default:
            break;
        }

        const Option<T> *opt = (idx == -1 ? shortOptions.value(c) : longOptions.value(idx));
        assert(opt);
        assert(opt->option);
        ret = handler(opt->option);
    }
    if (optind < argc) {
        fprintf(stderr, "unexpected option -- '%s'\n", argv[optind]);
        return Parse_Error;
    }

    return ret;
}
template <typename T>
static void help(FILE *f, const char* app, const Option<T> *opts, size_t count)
{
    List<String> out;
    int longest = 0;
    for (size_t i=0; i<count; ++i) {
        if (!opts[i].longOpt && !opts[i].shortOpt) {
            out.append(String());
        } else {
            out.append(String::format<64>("  %s%s%s%s",
                                          opts[i].longOpt ? String::format<4>("--%s", opts[i].longOpt).constData() : "",
                                          opts[i].longOpt && opts[i].shortOpt ? "|" : "",
                                          opts[i].shortOpt ? String::format<2>("-%c", opts[i].shortOpt).constData() : "",
                                          opts[i].argument == required_argument ? " [arg] "
                                          : opts[i].argument == optional_argument ? " [optional] " : ""));
            longest = std::max<int>(out[i].size(), longest);
        }
    }
    fprintf(f, "%s options...\n", app);
    const size_t c = out.size();
    for (size_t i=0; i<c; ++i) {
        if (out.at(i).isEmpty()) {
            fprintf(f, "%s\n", opts[i].description.constData());
        } else {
            fprintf(f, "%s%s %s\n",
                    out.at(i).constData(),
                    String(longest - out.at(i).size(), ' ').constData(),
                    opts[i].description.constData());
        }
    }
}

template <typename T>
static void man(const Option<T> *opts, size_t count)
{
    String out = ("<!DOCTYPE manpage SYSTEM \"http://masqmail.cx/xmltoman/xmltoman.dtd\">\n"
                  "<?xml-stylesheet type=\"text/xsl\" href=\"http://masqmail.cx/xmltoman/xmltoman.xsl\"?>\n"
                  "\n"
                  "<manpage name=\"rc\" section=\"1\" desc=\"command line client for RTags\">\n"
                  "\n"
                  "<synopsis>\n"
                  "  <cmd>rc <arg>file.1.xml</arg> > file.1</cmd>\n"
                  "</synopsis>\n"
                  "\n"
                  "<description>\n"
                  "\n"
                  "<p>rc is a command line client used to control RTags.</p>\n"
                  "\n"
                  "</description>\n");
    for (size_t i=0; i<count; ++i) {
        if (!opts[i].description.isEmpty()) {
            if (!opts[i].longOpt && !opts[i].shortOpt) {
                if (i)
                    out.append("</section>\n");
                out.append(String::format<128>("<section name=\"%s\">\n", opts[i].description.constData()));
            } else {
                out.append(String::format<64>("  <option>%s%s%s%s<optdesc>%s</optdesc></option>\n",
                                              opts[i].longOpt ? String::format<4>("--%s", opts[i].longOpt).constData() : "",
                                              opts[i].longOpt && opts[i].shortOpt ? "|" : "",
                                              opts[i].shortOpt ? String::format<2>("-%c", opts[i].shortOpt).constData() : "",
                                              opts[i].argument == required_argument ? " [arg] "
                                              : opts[i].argument == optional_argument ? " [optional] " : "",
                                              opts[i].description.constData()));
            }
        }
    }
    out.append("</section>\n"
               "<section name=\"Authors\">\n"
               "  <p>RTags was written by Jan Erik Hanssen &lt;jhanssen@gmail.com&gt; and Anders Bakken &lt;abakken@gmail.com&gt;</p>\n"
               "</section>\n"
               "<section name=\"See also\">\n"
               "  <p><manref name=\"rdm\" section=\"1\"/></p>\n"
               "</section>\n"
               "<section name=\"Comments\">\n"
               "  <p>This man page was written using <manref name=\"xmltoman\" section=\"1\" href=\"http://masqmail.cx/xmltoman/\"/>.</p>\n"
               "</section>\n"
               "</manpage>\n");
    printf("%s", out.constData());
}
}


struct CompilationDataBaseInfo {
    uint64_t lastModified;
    List<String> environment;
    Flags<IndexMessage::Flag> indexFlags;
};

inline Serializer &operator<<(Serializer &s, const CompilationDataBaseInfo &info)
{
    s << info.lastModified << Sandbox::encoded(info.environment) << info.indexFlags;
    return s;
}

inline Deserializer &operator>>(Deserializer &s, CompilationDataBaseInfo &info)
{
    s >> info.lastModified >> info.environment >> info.indexFlags;
    Sandbox::decode(info.environment);
    return s;
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
inline Log operator<<(Log dbg, CXType type);
inline Log operator<<(Log dbg, CXCursorKind kind);
inline Log operator<<(Log dbg, CXTypeKind kind);
inline Log operator<<(Log dbg, CXLinkageKind kind);

inline bool operator==(const CXCursor &l, const CXCursor &r) { return clang_equalCursors(l, r); }
inline bool operator!=(const CXCursor &l, const CXCursor &r) { return !clang_equalCursors(l, r); }
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
