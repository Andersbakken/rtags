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

#include "RTags.h"

#include <dirent.h>
#include <fcntl.h>
#include <fnmatch.h>
#include <sys/types.h>
#ifdef OS_FreeBSD
#include <sys/sysctl.h>
#endif
#ifdef OS_Darwin
#include <mach-o/dyld.h>
#endif

#include "IndexDataMessage.h"
#include "LogOutputMessage.h"
#include "QueryMessage.h"
#include "rct/Rct.h"
#include "rct/Connection.h"
#include "rct/StopWatch.h"
#include "Server.h"
#include "ClangIndexer.h"
#include "Project.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include "RTagsVersion.h"
#include <clang-c/CXCompilationDatabase.h>

namespace RTags {
String versionString()
{
    return String::format<64>("%d.%d.%d", MajorVersion, MinorVersion, DatabaseVersion);
}

void encodePath(Path &path)
{
    Sandbox::encode(path);
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

void decodePath(Path &path)
{
    Sandbox::decode(path);
    int i = 0;
    int size = path.size();
    while (i < size) {
        char &ch = path[i];
        if (ch == '_') {
            if (i + 1 < size && path.at(i + 1) == '_') {
                path.remove(i + 1, 1);
                --size;
            } else {
                ch = '/';
            }
        }
        ++i;
    }
}


Path encodeSourceFilePath(const Path &dataDir, const Path &project, uint32_t fileId)
{
    String str = dataDir;
    Path p = project;
    encodePath(p);
    str << p << '/';
    if (fileId)
        str << fileId << '/';
    return str;
}

Path findAncestor(Path path, const String &fn, Flags<FindAncestorFlag> flags, SourceCache *cache)
{
    Path *cacheResult = 0;
    if (cache) {
        const Path parent = path.parentDir();
        cacheResult = &cache->ancestorCache[parent][SourceCache::AncestorCacheKey { fn, flags }];
        if (!cacheResult->isEmpty()) {
            return *cacheResult;
        }
    }
    Path ret;
    char buf[PATH_MAX + sizeof(dirent) + 1];
    int slash = path.size();
    const int len = fn.size() + 1;
    struct stat st;

    memcpy(buf, path.constData(), path.size() + 1);
    while ((slash = path.lastIndexOf('/', slash - 1)) > 0) { // We don't want to search in /
        if (!(flags & Wildcard)) {
            memcpy(buf + slash + 1, fn.constData(), len);
            if (!stat(buf, &st)) {
                buf[slash + 1] = '\0';
                ret = buf;
                if (flags & Shallow) {
                    break;
                }
            }
        } else {
            buf[slash + 1] = '\0';
            DIR *dir = opendir(buf);
            bool found = false;
            if (dir) {
                while (dirent *entry = readdir(dir)) {
                    const int l = strlen(entry->d_name) + 1;
                    switch (l - 1) {
                    case 1:
                        if (entry->d_name[0] == '.')
                            continue;
                        break;
                    case 2:
                        if (entry->d_name[0] == '.' && entry->d_name[1] == '.')
                            continue;
                        break;
                    }
                    assert(buf[slash] == '/');
                    assert(l + slash + 1 < static_cast<int>(sizeof(buf)));
                    memcpy(buf + slash + 1, entry->d_name, l);
                    if (!fnmatch(fn.constData(), buf, 0)) {
                        ret = buf;
                        ret.truncate(slash + 1);
                        found = true;
                        break;
                    }
                }
                closedir(dir);
            }
            if (found && flags & Shallow)
                break;
        }
    }

    ret = ret.ensureTrailingSlash();
    if (cacheResult) {
        *cacheResult = ret;
    }
    return ret;
}

Map<String, String> rtagsConfig(const Path &path, SourceCache *cache)
{
    Path dir = path.isDir() ? path : path.parentDir();
    Map<String, String> ret;
    char buf[1024];
    while (dir.size() > 1) {
        assert(dir.endsWith('/'));
        if (cache) {
            auto it = cache->rtagsConfigCache.find(dir);
            if (it != cache->rtagsConfigCache.end()) {
                for (const auto entry : it->second) {
                    auto &ref = ret[entry.first];
                    if (ref.isEmpty())
                        ref = entry.second;
                }
                dir = dir.parentDir();
                continue;
            }
        }
        Map<String, String> *cacheEntry = 0;
        if (cache) {
            cacheEntry = &cache->rtagsConfigCache[dir];
            // we want to cache empty entries
        }
        snprintf(buf, sizeof(buf), "%s.rtags-config", dir.constData());
        if (FILE *f = fopen(buf, "r")) {
            while ((fgets(buf, sizeof(buf), f))) {
                int len = strlen(buf);
                while (len > 0 && isspace(buf[len - 1]))
                    --len;
                if (len) {
                    buf[len] = '\0';
                    String key;
                    char *colon = strchr(buf, ':');
                    char *value = 0;
                    if (colon) {
                        key.assign(buf, colon - buf);
                        value = colon + 1;
                        while (isspace(*value))
                            ++value;
                    } else {
                        key.assign(buf, len);
                    }
                    if (!key.isEmpty()) {
                        if (!ret.contains(key))
                            ret[key] = value;
                        if (cacheEntry)
                            (*cacheEntry)[key] = value;
                    }
                }
            }
            fclose(f);
        }
        dir = dir.parentDir();
    }
    return ret;
}

struct Entry {
    const char *name;
    const Flags<FindAncestorFlag> flags;
};

static inline Path checkEntries(const Entry *entries, const Path &path, const Path &home, SourceCache *cache)
{
    Path best;
    for (int i=0; entries[i].name; ++i) {
        Path p = findAncestor(path, entries[i].name, entries[i].flags, cache);
        if ((p.isEmpty() || p == home) && (entries[i].flags & Wildcard)) {
            const int len = strlen(entries[i].name);
            if (entries[i].name[len - 1] == '*') {
                const String name(entries[i].name, len - 1);
                p = findAncestor(path, name.constData(), entries[i].flags & ~Wildcard, cache);
            }
        }
        if (p.isEmpty() || p == home)
            continue;
        if (entries[i].flags & Authoritative) {
            best = p;
            break;
        } else if (best.isEmpty() || p.size() < best.size()) {
            best = p;
        }
    }
    return best;
}


Path findProjectRoot(const Path &path, ProjectRootMode mode, SourceCache *cache)
{
    if (path == "-")
        return Path();
    if (!path.isAbsolute())
        error() << "GOT BAD PATH" << path;
    assert(path.isAbsolute());
    const Map<String, String> config = rtagsConfig(path, cache);
    {
        const Path project = config.value("project");
        if (!project.isEmpty() && project.isDir())
            return project.ensureTrailingSlash();
    }

    Path ret;
    static const Path home = Path::home();
    if (mode == SourceRoot) {
        const Entry before[] = {
            { ".projectile", Authoritative },
            { ".git", Flags<FindAncestorFlag>() },
            { ".hg", Flags<FindAncestorFlag>() },
            { ".svn", Flags<FindAncestorFlag>() },
            { ".fslckout", Flags<FindAncestorFlag>() },
            { "_FOSSIL_", Flags<FindAncestorFlag>() },
            { "_darcs_", Flags<FindAncestorFlag>() },
            { ".bzr", Flags<FindAncestorFlag>() },
            { ".tup", Flags<FindAncestorFlag>() },
            { "GTAGS", Flags<FindAncestorFlag>() },
            { "TAGS", Flags<FindAncestorFlag>() },
            { "configure", Flags<FindAncestorFlag>() },
            { "CMakeLists.txt", Flags<FindAncestorFlag>() },
            { "*.pro", Wildcard },
            { "scons.1", Flags<FindAncestorFlag>() },
            { "*.scons", Wildcard },
            { "SConstruct", Flags<FindAncestorFlag>() },
            { "autogen.*", Wildcard },
            { "GNUMakefile*", Wildcard },
            { "INSTALL*", Wildcard },
            { "README*", Wildcard },
            { 0, Flags<FindAncestorFlag>() }
        };
        {
            const Path e = checkEntries(before, path, home, cache);
            if (!e.isEmpty() && (e.size() < ret.size() || ret.isEmpty()))
                ret = e;
        }
    }
    if (!ret.isEmpty())
        return ret;
    {
        const Path configStatus = findAncestor(path, "config.status", Flags<FindAncestorFlag>(), cache);
        if (!configStatus.isEmpty()) {
            if (mode == BuildRoot)
                return configStatus;
            FILE *f = fopen((configStatus + "config.status").constData(), "r");
            if (f) {
                char line[1024];
                enum { MaxLines = 10 };
                for (int i=0; i<MaxLines; ++i) {
                    int r = Rct::readLine(f, line, sizeof(line));
                    if (r == -1)
                        break;
                    char *configure = strstr(line, "/configure ");
                    if (configure) {
                        char *end = configure + 10;
                        while (--configure >= line) {
                            Path conf(configure, end - configure);
                            if (!conf.isAbsolute())
                                conf.resolve();
                            if (conf.isFile()) {
                                ret = conf.parentDir();
                                if (ret == home)
                                    ret.clear();
                                break;
                            }
                        }
                    }
                    if (!ret.isEmpty())
                        break;
                }
                fclose(f);
                if (!ret.isEmpty())
                    return ret;
            }
        }
    }
    {
        const Path cmakeCache = findAncestor(path, "CMakeCache.txt", Flags<FindAncestorFlag>(), cache);
        if (!cmakeCache.isEmpty()) {
            if (mode == BuildRoot)
                return cmakeCache;
            FILE *f = fopen((cmakeCache + "Makefile").constData(), "r");
            bool makefile = true;
            if (!f) {
                f = fopen((cmakeCache + "build.ninja").constData(), "r");
                makefile = false;
            }
            if (f) {
                char line[1024];
                enum { MaxLines = 256 };
                for (int i=0; i<MaxLines; ++i) {
                    int r = Rct::readLine(f, line, sizeof(line));
                    if (r == -1) {
                        break;
                    }

                    if (makefile) {
                        if (!strncmp(line, "CMAKE_SOURCE_DIR", 16)) {
                            char *dir = line + 16;
                            while (*dir && (*dir == ' ' || *dir == '='))
                                ++dir;
                            if (dir != home) {
                                ret = dir;
                                ret += '/';
                                if (!Path(ret + "CMakeLists.txt").isFile())
                                    ret.clear();
                            }
                            break;
                        }
                    } else if (!strncmp(line, "# Write statements declared in CMakeLists.txt:", 46)) {
                        r = Rct::readLine(f, line, sizeof(line));
                        ++i;
                        if (r != -1) {
                            ret = line + 2;
                            if (ret.exists()) {
                                ret = ret.parentDir();
                                break;
                            } else {
                                ret.clear();
                            }
                        }
                    }
                }
                fclose(f);
                if (!ret.isEmpty())
                    return ret;
            }
        }
    }
    const Entry after[] = {
        { "build.ninja", Flags<FindAncestorFlag>() },
        { "Makefile*", Wildcard },
        { "compile_commands.json", Flags<FindAncestorFlag>() },
        { 0, Flags<FindAncestorFlag>() }
    };

    {
        const Path e = checkEntries(after, path, home, cache);
        if (!e.isEmpty() && (e.size() < ret.size() || ret.isEmpty()))
            ret = e;
    }

    if (!ret.isEmpty())
        return ret;

    if (mode == BuildRoot)
        return findProjectRoot(path, SourceRoot, cache);

    return Path();
}

void initMessages()
{
    Message::registerMessage<IndexMessage>();
    Message::registerMessage<IndexDataMessage>();
    Message::registerMessage<LogOutputMessage>();
    Message::registerMessage<QueryMessage>();
    Message::registerMessage<VisitFileMessage>();
    Message::registerMessage<VisitFileResponseMessage>();
}
String eatString(CXString str)
{
    const String ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

String cursorToString(CXCursor cursor, Flags<CursorToStringFlags> flags)
{
    const CXCursorKind kind = clang_getCursorKind(cursor);
    String ret;
    ret.reserve(256);
    ret += eatString(clang_getCursorKindSpelling(kind));
    if (clang_isInvalid(kind))
        return ret;

    switch (RTags::cursorType(kind)) {
    case Type_Reference:
        ret += " ref";
        break;
    case Type_Cursor:
        ret += " cursor";
        break;
    case Type_Other:
        ret += " other";
        break;
    case Type_Statement:
        ret += " statement";
        break;
    case Type_Include:
        ret += " include";
        break;
    case Type_Literal:
        ret += " literal";
        break;
    }

    const String name = eatString(clang_getCursorDisplayName(cursor));
    const String other = eatString(clang_getCursorSpelling(cursor));
    if (!name.isEmpty())
        ret += " " + name;
    if (other != name && !other.isEmpty())
        ret += " " + other;

    if (clang_isCursorDefinition(cursor))
        ret += " def";

    if (flags & IncludeUSR) {
        const String usr = eatString(clang_getCursorUSR(clang_getCanonicalCursor(cursor)));
        if (!usr.isEmpty()) {
            ret += " " + usr;
        }
    }

    if (flags & IncludeSpecializedUsr) {
        const CXCursor general = clang_getSpecializedCursorTemplate(cursor);
        if (!clang_Cursor_isNull(general)) {
            const String usr = eatString(clang_getCursorUSR(clang_getCanonicalCursor(general)));
            if (!usr.isEmpty()) {
                ret += " " + usr;
            }
        }
    }

    CXString file;
    unsigned int line, col;
    for (int pieceIndex = 0; true; ++pieceIndex) {
        CXSourceRange range = clang_Cursor_getSpellingNameRange(cursor, pieceIndex, 0);
        if (clang_Range_isNull(range))
            break;
        CXSourceLocation rangeStart = clang_getRangeStart(range);
        clang_getPresumedLocation(rangeStart, &file, &line, &col);

        const char *data = clang_getCString(file);
        if (data && *data) {
            ret += ' ';
            ret += data;
            ret += ':';
            ret += String::number(line);
            ret += ':';
            ret += String::number(col);
        }
        clang_disposeString(file);
    }
    return ret;
}

std::shared_ptr<TranslationUnit> TranslationUnit::create(const Path &sourceFile, const List<String> &args,
                                                         CXUnsavedFile *unsaved, int unsavedCount,
                                                         Flags<CXTranslationUnit_Flags> translationUnitFlags,
                                                         bool displayDiagnostics)

{
    auto ret = std::make_shared<TranslationUnit>();
    ret->clangLine = "clang ";
    ret->index = clang_createIndex(0, displayDiagnostics);

    int idx = 0;
    List<const char*> clangArgs(args.size() + 2, 0);

    const int count = args.size();
    for (int j=0; j<count; ++j) {
        clangArgs[idx++] = args.at(j).constData();
        String arg = args.at(j);
        arg.replace("\"", "\\\"");
        ret->clangLine += '"' + arg + '"';
        ret->clangLine += ' ';
    }
    // clangArgs[idx++] = "-disable-free";
    // clangArgs[idx++] = "-disable-llvm-verifier";

    ret->clangLine += sourceFile;

    // StopWatch sw;
#if CINDEX_VERSION_MINOR >= 23
    for (int i=0; i<3; ++i) {
        auto error = clang_parseTranslationUnit2(ret->index, sourceFile.constData(),
                                                 clangArgs.data(), idx, unsaved, unsavedCount,
                                                 translationUnitFlags.cast<unsigned int>(), &ret->unit);
        if (error != CXError_Crashed)
            break;
        usleep(100000);
    }
#else
    ret->unit = clang_parseTranslationUnit(ret->index, sourceFile.constData(),
                                           clangArgs.data(), idx, unsaved, unsavedCount,
                                           translationUnitFlags.cast<unsigned int>());
#endif
    // error() << sourceFile << sw.elapsed();
    return ret;
}

bool TranslationUnit::reparse(CXUnsavedFile *unsaved, int unsavedCount)
{
    assert(unit);
    if (clang_reparseTranslationUnit(unit, unsavedCount, unsaved, clang_defaultReparseOptions(unit)) != 0) {
        clang_disposeTranslationUnit(unit);
        unit = 0;
        return false;
    }
    return true;
}

#if 1
struct No
{
    template<typename T>
    No &operator<<(const T &) { return *this; }
};
#define l() No()
#else
#define l() error()
#endif

bool resolveAuto(const CXCursor &cursor, Auto *a)
{
    CXType type = clang_getCursorType(cursor);
    while (type.kind == CXType_Pointer)
        type = clang_getPointeeType(type);

    if (
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 32)
        type.kind != CXType_Auto
#else
        type.kind != CXType_Unexposed || RTags::eatString(clang_getTypeSpelling(type)) != "auto"
#endif
        ) {
        return false;
    }

    if (a) {
        a->type = clang_getCanonicalType(type);
        a->cursor = clang_getTypeDeclaration(a->type);
    }
    return true;
}

#undef l

static CXChildVisitResult findFirstChildVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    *reinterpret_cast<CXCursor*>(data) = cursor;
    return CXChildVisit_Break;
}

CXCursor findFirstChild(CXCursor parent)
{
    CXCursor ret = clang_getNullCursor();
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findFirstChildVisitor, &ret);
    return ret;
}

struct FindChildVisitor
{
    CXCursorKind kind;
    String name;
    CXCursor cursor;
};

static CXChildVisitResult findChildVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    FindChildVisitor *u = reinterpret_cast<FindChildVisitor*>(data);
    if (u->name.isEmpty()) {
        if (clang_getCursorKind(cursor) == u->kind) {
            u->cursor = cursor;
            return CXChildVisit_Break;
        }
    } else {
        CXStringScope str = clang_getCursorSpelling(cursor);
        if (str.data() && u->name == str.data()) {
            u->cursor = cursor;
            return CXChildVisit_Break;
        }
    }
    return CXChildVisit_Continue;
}

CXCursor findChild(CXCursor parent, CXCursorKind kind)
{
    FindChildVisitor u = { kind, String(), clang_getNullCursor() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findChildVisitor, &u);
    return u.cursor;
}

CXCursor findChild(CXCursor parent, const String &name)
{
    FindChildVisitor u = { CXCursor_FirstInvalid, name, clang_getNullCursor() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findChildVisitor, &u);
    return u.cursor;
}

struct ChildrenVisitor
{
    const Filter &in;
    const Filter &out;
    List<CXCursor> children;
};

static CXChildVisitResult childrenVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    ChildrenVisitor *u = reinterpret_cast<ChildrenVisitor*>(data);
    if ((u->out.isNull() || !u->out.match(cursor)) && (u->in.isNull() || u->in.match(cursor))) {
        u->children.append(cursor);
    }
    return CXChildVisit_Continue;
}

List<CXCursor> children(CXCursor parent, const Filter &in, const Filter &out)
{
    ChildrenVisitor userData = { in, out, List<CXCursor>() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, childrenVisitor, &userData);
    return userData.children;
}

struct FindChainVisitor
{
    const List<CXCursorKind> &kinds;
    List<CXCursor> ret;
};

static CXChildVisitResult findChainVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    FindChainVisitor *u = reinterpret_cast<FindChainVisitor*>(data);
    if (clang_getCursorKind(cursor) == u->kinds.at(u->ret.size())) {
        u->ret.append(cursor);
        if (u->ret.size() < u->kinds.size())
            return CXChildVisit_Recurse;

        return CXChildVisit_Break;
    }
    return CXChildVisit_Break;
}

List<CXCursor> findChain(CXCursor parent, const List<CXCursorKind> &kinds)
{
    assert(!kinds.isEmpty());
    FindChainVisitor userData = { kinds, List<CXCursor>() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findChainVisitor, &userData);
    if (userData.ret.size() != kinds.size()) {
        userData.ret.clear();
    }
    return userData.ret;
}

String typeName(const CXCursor &cursor)
{
    String ret;
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_FunctionTemplate:
        // ### If the return value is a template type we get an empty string here
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
        ret = typeString(clang_getResultType(clang_getCursorType(cursor)));
        break;
    case CXCursor_ClassTemplate:
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_UnionDecl:
    case CXCursor_TypedefDecl:
    case CXCursor_EnumDecl:
        ret = RTags::eatString(clang_getCursorSpelling(cursor));
        break;
    case CXCursor_VarDecl: {
        const CXCursor initType = RTags::findFirstChild(cursor);
        if (clang_getCursorKind(initType) == CXCursor_InitListExpr) {
            ret = typeString(clang_getCursorType(initType));
        } else {
            ret = typeString(clang_getCursorType(cursor));
        }
        break; }
    case CXCursor_FieldDecl: // ### If the return value is a template type we get an empty string here
    case CXCursor_ParmDecl:
        ret = typeString(clang_getCursorType(cursor));
        break;
    default:
        return String();
    }
    if (!ret.isEmpty() && !ret.endsWith('*') && !ret.endsWith('&'))
        ret.append(' ');
    return ret;
}

String typeString(const CXType &type)
{
    String ret;
    if (clang_isConstQualifiedType(type))
        ret = "const ";

    const char *builtIn = builtinTypeName(type.kind);
    if (builtIn) {
        ret += builtIn;
        return ret;
    }

    if (char pointer = (type.kind == CXType_Pointer ? '*' : (type.kind == CXType_LValueReference ? '&' : 0))) {
        const CXType pointee = clang_getPointeeType(type);
        ret += typeString(pointee);
        if (ret.endsWith('*') || ret.endsWith('&')) {
            ret += pointer;
        } else {
            ret += ' ';
            ret += pointer;
        }
        return ret;
    }

    if (type.kind == CXType_ConstantArray) {
        ret += typeString(clang_getArrayElementType(type));
        const int64_t count = clang_getNumElements(type);
        ret += '[';
        if (count >= 0)
            ret += String::number(count);
        ret += ']';
        return ret;
    }
    ret += typeName(clang_getTypeDeclaration(type));
    if (ret.endsWith(' '))
        ret.chop(1);
    return ret;
}

#define OUTPUT_LITERAL(string)                  \
    {                                           \
        const char literal[] = string;          \
        output(literal, sizeof(literal) - 1);   \
    }
class ElispFormatter : public Value::Formatter
{
public:
    virtual void format(const Value &value, std::function<void(const char *, size_t)> output) const
    {
        switch (value.type()) {
        case Value::Type_Invalid:
        case Value::Type_Undefined:
            OUTPUT_LITERAL("nil");
            break;
        case Value::Type_Boolean:
            if (value.toBool()) {
                OUTPUT_LITERAL("t");
            } else {
                OUTPUT_LITERAL("nil");
            }
            break;
        case Value::Type_Integer: {
            char buf[128];
            const size_t w = snprintf(buf, sizeof(buf), "%d", value.toInteger());
            output(buf, w);
            break; }
        case Value::Type_Double: {
            char buf[128];
            const size_t w = snprintf(buf, sizeof(buf), "%g", value.toDouble());
            output(buf, w);
            break; }
        case Value::Type_String: {
            const String str = elispEscape(value.toString());
            OUTPUT_LITERAL("\"");
            output(str.constData(), str.size());
            OUTPUT_LITERAL("\"");
            break; }
        case Value::Type_Custom: {
            const String str = elispEscape(value.toCustom()->toString());
            OUTPUT_LITERAL("\"");
            output(str.constData(), str.size());
            OUTPUT_LITERAL("\"");
            break; }
        case Value::Type_Map: {
            const auto end = value.end();
            bool first = true;
            output("(list ", 6);
            for (auto it = value.begin(); it != end; ++it) {
                if (!first) {
                    output(" ", 1);
                } else {
                    first = false;
                }
                OUTPUT_LITERAL("(cons '");
                output(it->first.constData(), it->first.size());
                OUTPUT_LITERAL(" ");
                format(it->second, output);
                OUTPUT_LITERAL(")");
            }
            output(")", 1);
            break; }
        case Value::Type_List: {
            const auto end = value.listEnd();
            OUTPUT_LITERAL("(list ");
            bool first = true;
            for (auto it = value.listBegin(); it != end; ++it) {
                if (!first) {
                    OUTPUT_LITERAL(" ");
                } else {
                    first = false;
                }
                format(*it, output);
            }
            OUTPUT_LITERAL(")");
            break; }
        case Value::Type_Date:
            const String str = elispEscape(String::formatTime(value.toDate().time()));
            OUTPUT_LITERAL("\"");
            output(str.constData(), str.size());
            OUTPUT_LITERAL("\"");
            break;
        }
    }
};
String toElisp(const Value &value)
{
    return ElispFormatter().toString(value);
}
}

