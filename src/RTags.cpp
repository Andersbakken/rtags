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
#include "rct/StopWatch.h"
#include "Server.h"
#include "ClangIndexer.h"
#include "Project.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"

#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR > 3)
#include <clang-c/CXCompilationDatabase.h>
#endif


namespace RTags {
void encodePath(Path &path)
{
    // SBROOT
    if (!Location::sandboxRoot().isEmpty()) {
        Location::convertPathRelative(path);
    }
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
    // SBROOT
    if (!Location::sandboxRoot().isEmpty()) {
        Location::strPathToSbRoot(path);
    }
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


Path findAncestor(Path path, const char *fn, Flags<FindAncestorFlag> flags = Flags<FindAncestorFlag>())
{
    Path ret;
    int slash = path.size();
    const int len = strlen(fn) + 1;
    struct stat st;
    char buf[PATH_MAX + sizeof(dirent) + 1];
    dirent *direntBuf = 0, *entry = 0;
    if (flags & Wildcard)
        direntBuf = reinterpret_cast<struct dirent *>(malloc(sizeof(buf)));

    memcpy(buf, path.constData(), path.size() + 1);
    while ((slash = path.lastIndexOf('/', slash - 1)) > 0) { // We don't want to search in /
        if (!(flags & Wildcard)) {
            memcpy(buf + slash + 1, fn, len);
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
                while (!readdir_r(dir, direntBuf, &entry) && entry) {
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
                    if (!fnmatch(fn, buf, 0)) {
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
    if (flags & Wildcard)
        free(direntBuf);

    return ret.ensureTrailingSlash();
}

Map<String, String> rtagsConfig(const Path &path)
{
    Path dir = path.isDir() ? path : path.parentDir();
    Map<String, String> ret;
    if (dir.resolve()) {
        char buf[1024];
        struct stat statBuf;
        while (dir.size() > 1) {
            assert(dir.endsWith('/'));
            snprintf(buf, sizeof(buf), "%s.rtags-config", dir.constData());
            if (!stat(buf, &statBuf) && S_ISREG(statBuf.st_mode)) {
                FILE *f = fopen(buf, "r");
                if (f) {
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
                            if (!key.isEmpty() && !ret.contains(key)) {
                                ret[key] = value;
                            }
                        }
                    }
                    fclose(f);
                }
            }
            dir = dir.parentDir();
        }
    }
    return ret;
}

struct Entry {
    const char *name;
    const Flags<FindAncestorFlag> flags;
};

static inline Path checkEntries(const Entry *entries, const Path &path, const Path &home)
{
    Path best;
    for (int i=0; entries[i].name; ++i) {
        Path p = findAncestor(path, entries[i].name, entries[i].flags);
        if ((p.isEmpty() || p == home) && (entries[i].flags & Wildcard)) {
            const int len = strlen(entries[i].name);
            if (entries[i].name[len - 1] == '*') {
                const String name(entries[i].name, len - 1);
                p = findAncestor(path, name.constData(), entries[i].flags & ~Wildcard);
            }
        }
        if (!p.isEmpty() && p != home && (best.isEmpty() || p.size() < best.size())) {
            best = p;
        }
    }
    return best;
}


Path findProjectRoot(const Path &path, ProjectRootMode mode)
{
    if (!path.isAbsolute())
        error() << "GOT BAD PATH" << path;
    assert(path.isAbsolute());
    const Map<String, String> config = rtagsConfig(path);
    {
        const Path project = config.value("project");
        if (!project.isEmpty() && project.isDir())
            return project.ensureTrailingSlash();
    }

    Path ret;
    static const Path home = Path::home();
    if (mode == SourceRoot) {
        const Entry before[] = {
            { ".git", Flags<FindAncestorFlag>() },
            { ".svn", Flags<FindAncestorFlag>() },
            { ".bzr", Flags<FindAncestorFlag>() },
            { ".tup", Flags<FindAncestorFlag>() },
            { "GTAGS", Flags<FindAncestorFlag>() },
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
            const Path e = checkEntries(before, path, home);
            if (!e.isEmpty() && (e.size() < ret.size() || ret.isEmpty()))
                ret = e;
        }
    }
    if (!ret.isEmpty())
        return ret;
    {
        const Path configStatus = findAncestor(path, "config.status");
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
        const Path cmakeCache = findAncestor(path, "CMakeCache.txt");
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
        const Path e = checkEntries(after, path, home);
        if (!e.isEmpty() && (e.size() < ret.size() || ret.isEmpty()))
            ret = e;
    }

    if (!ret.isEmpty())
        return ret;

    if (mode == BuildRoot)
        return findProjectRoot(path, SourceRoot);

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

void parseTranslationUnit(const Path &sourceFile, const List<String> &args,
                          CXTranslationUnit &unit, CXIndex index,
                          CXUnsavedFile *unsaved, int unsavedCount,
                          Flags<CXTranslationUnit_Flags> translationUnitFlags,
                          String *clangLine)

{
    if (clangLine)
        *clangLine = "clang ";

    int idx = 0;
    List<const char*> clangArgs(args.size() + 2, 0);

    const int count = args.size();
    for (int j=0; j<count; ++j) {
        clangArgs[idx++] = args.at(j).constData();
        if (clangLine) {
            String arg = args.at(j);
            arg.replace("\"", "\\\"");
            *clangLine += '"' + arg + '"';
            *clangLine += ' ';
        }
    }
    // clangArgs[idx++] = "-disable-free";
    // clangArgs[idx++] = "-disable-llvm-verifier";

    if (clangLine)
        *clangLine += sourceFile;

    // StopWatch sw;
#if CINDEX_VERSION_MINOR >= 23
    for (int i=0; i<3; ++i) {
        auto error = clang_parseTranslationUnit2(index, sourceFile.constData(),
                                                 clangArgs.data(), idx, unsaved, unsavedCount,
                                                 translationUnitFlags.cast<unsigned int>(), &unit);
        if (error != CXError_Crashed)
            break;
        usleep(100000);
    }
#else
    unit = clang_parseTranslationUnit(index, sourceFile.constData(),
                                      clangArgs.data(), idx, unsaved, unsavedCount,
                                      translationUnitFlags.cast<unsigned int>());
#endif
    // error() << sourceFile << sw.elapsed();
}

void reparseTranslationUnit(CXTranslationUnit &unit, CXUnsavedFile *unsaved, int unsavedCount)
{
    assert(unit);
    if (clang_reparseTranslationUnit(unit, unsavedCount, unsaved, clang_defaultReparseOptions(unit)) != 0) {
        clang_disposeTranslationUnit(unit);
        unit = 0;
    }
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

std::shared_ptr<Auto> resolveAuto(const CXCursor &cursor)
{
    std::shared_ptr<Auto> ret;
    CXType type = clang_getCursorType(cursor);
    while (type.kind == CXType_Pointer)
        type = clang_getPointeeType(type);

    if (type.kind == CXType_Unexposed) {
        const CXStringScope spelling = clang_getTypeSpelling(type);
        if (!strcmp(clang_getCString(spelling), "auto")) {
            const CXCursor null = clang_getNullCursor();
            ret.reset(new Auto { null, clang_getCursorType(null) });
            l() << "resolving" << cursor << clang_getCursorType(cursor).kind;
            assert(clang_getCursorKind(cursor) == CXCursor_VarDecl);
            CXCursor firstChild = findFirstChild(cursor);
            // error() << clang_getCursorKind(firstChild) << "fobar" << cursor;
            switch (clang_getCursorKind(firstChild)) {
            case CXCursor_IntegerLiteral:
            case CXCursor_FloatingLiteral:
            case CXCursor_ImaginaryLiteral:
            case CXCursor_StringLiteral:
            case CXCursor_CharacterLiteral:
                ret->type = clang_getCursorType(firstChild);
                break;
            case CXCursor_CXXNewExpr: {
                const CXCursor typeRef = findFirstChild(firstChild);
                if (clang_getCursorKind(typeRef) == CXCursor_TypeRef) {
                    ret->cursor = clang_getCursorReferenced(typeRef);
                    ret->type = clang_getCursorType(ret->cursor);
                }
                break; }
            case CXCursor_LambdaExpr:
                ret->cursor = firstChild;
                ret->type = clang_getCursorType(ret->cursor);
                break;
            case CXCursor_UnexposedExpr:
            case CXCursor_UnaryOperator:
            case CXCursor_MemberRefExpr:
            case CXCursor_ArraySubscriptExpr:
            case CXCursor_DeclRefExpr:
            case CXCursor_BinaryOperator:
            case CXCursor_CXXReinterpretCastExpr:
            case CXCursor_CXXStaticCastExpr:
            case CXCursor_CXXDynamicCastExpr:
            case CXCursor_CStyleCastExpr:
            case CXCursor_CXXConstCastExpr:
            case CXCursor_CallExpr: {
                CXCursor callExpr = firstChild;
                CXCursor func = clang_getNullCursor();
                CXCursor parent = func;
                bool done = false;
                do {
                    const CXCursorKind kind = clang_getCursorKind(callExpr);
                    // error() << "GOT" << kind << clang_getCursorKind(parent);
                    switch (kind) {
                    case CXCursor_CallExpr:
                        func = clang_getCursorReferenced(callExpr);
                        if (!clang_isInvalid(clang_getCursorKind(func))) {
                            if (clang_getCursorKind(func) == CXCursor_Constructor) {
                                ret->cursor = clang_getCursorSemanticParent(func);
                                ret->type = clang_getCursorType(ret->cursor);
                            } else {
                                ret->type = clang_getResultType(clang_getCursorType(func));
                                ret->cursor = clang_getTypeDeclaration(ret->type);
                            }
                            done = true;
                        }
                        break;
                    case CXCursor_LambdaExpr:
                        ret->cursor = callExpr;
                        ret->type = clang_getCursorType(callExpr);
                        done = true;
                        break;
                    case CXCursor_TypeRef:
                    case CXCursor_MemberRefExpr:
                    case CXCursor_DeclRefExpr:
                        ret->cursor = clang_getCursorReferenced(callExpr);
                        ret->type = clang_getCursorType(ret->cursor);
                        done = true;
                        break;
                    case CXCursor_NamespaceRef: {
                        RTags::Filter filter;
                        filter.kinds.insert(CXCursor_TypeRef);
                        filter.kinds.insert(CXCursor_MemberRefExpr);
                        const CXCursor typeRef = children(parent, filter).value(0, clang_getNullCursor());
                        switch (clang_getCursorKind(typeRef)) {
                        case CXCursor_TypeRef:
                        case CXCursor_MemberRefExpr:
                            ret->cursor = clang_getCursorReferenced(typeRef);
                            ret->type = clang_getCursorType(ret->cursor);
                            break;
                        default:
                            break;
                        }
                        done = true;
                        break; }
                    case CXCursor_UnexposedExpr:
                    case CXCursor_UnaryOperator:
                    case CXCursor_BinaryOperator:
                    case CXCursor_ArraySubscriptExpr:
                    case CXCursor_CXXReinterpretCastExpr:
                    case CXCursor_CXXStaticCastExpr:
                    case CXCursor_CXXDynamicCastExpr:
                    case CXCursor_CXXConstCastExpr:
                    case CXCursor_CStyleCastExpr:
                        break;
                    default:
                        done = true;
                        break;
                    }
                    parent = callExpr;
                    callExpr = findFirstChild(callExpr);
                } while (!done);

                break; }
            case CXCursor_InvalidFile:
                break;
            default:
                error() << "Unknown damned auto!!!" << cursor << firstChild;
                break;
            }
        }
    }
    if (ret && ret->type.kind != CXType_Invalid)
        ret->type = clang_getCanonicalType(ret->type);
    return ret;
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
#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR >= 1)
        const int64_t count = clang_getNumElements(type);
        ret += '[';
        if (count >= 0)
            ret += String::number(count);
        ret += ']';
#endif
        return ret;
    }
    ret += typeName(clang_getTypeDeclaration(type));
    if (ret.endsWith(' '))
        ret.chop(1);
    return ret;
}

class CompileCommandsOperation
{
public:
    CompileCommandsOperation(const Hash<Path, CompilationDataBaseInfo> &i, const Path &p)
        : infos(i), projectRootOverride(p), indexIndex(0)
    {}
    ~CompileCommandsOperation()
    {
        if (project) {
            for (auto &info : infos) {
                info.second.lastModified = Path(info.first + "compile_commands.json").lastModifiedMs();
                if (info.second.indexFlags & IndexMessage::AppendCompilationDatabase) {
                    assert(infos.size() == 1);
                    info.second.indexFlags &= ~IndexMessage::AppendCompilationDatabase;
                    project->addCompilationDatabaseInfo(std::move(info.first), std::move(info.second));
                    return;
                }
            }
            project->setCompilationDatabaseInfos(std::move(infos), indexed);
        }
    }

    struct Index {
        String args;
        Path dir, compileCommandsDir;
    };

    void work()
    {
        Server *server = Server::instance();
        if (!server) {
            delete this;
            return;
        }
        const size_t max = std::min(indexIndex + 5, indexes.size());
        while (indexIndex < max) {
            const Index &idx = indexes.at(indexIndex);
            const auto it = infos.find(idx.compileCommandsDir);
            assert(it != infos.end());
            server->index(idx.args,
                          idx.dir,
                          it->second.pathEnvironment,
                          projectRootOverride,
                          it->second.indexFlags,
                          &project,
                          &indexed);
            ++indexIndex;
        }
        if (indexIndex == indexes.size()) {
            EventLoop::deleteLater(this);
        } else {
            EventLoop::eventLoop()->callLater(std::bind(&CompileCommandsOperation::work, this));
        }
    }
    Hash<Path, CompilationDataBaseInfo> infos;
    const Path projectRootOverride;
    Set<uint64_t> indexed;
    std::shared_ptr<Project> project;
    List<Index> indexes;
    size_t indexIndex;
};

bool loadCompileCommands(const Hash<Path, CompilationDataBaseInfo> &infos, const Path &projectRootOverride)
{
#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR > 3)
    CompileCommandsOperation *op = 0;
    for (const auto &info : infos) {
        CXCompilationDatabase_Error err;
        CXCompilationDatabase db = clang_CompilationDatabase_fromDirectory(info.first.constData(), &err);
        if (err != CXCompilationDatabase_NoError) {
            error("Can't load compilation database from %scompile_Commands.json", info.first.constData());
            continue;
        }
        CXCompileCommands cmds = clang_CompilationDatabase_getAllCompileCommands(db);
        const unsigned int sz = clang_CompileCommands_getSize(cmds);
        if (!op) {
            op = new CompileCommandsOperation(infos, projectRootOverride);
            op->indexes.reserve(sz);
        } else {
            op->indexes.reserve(op->indexes.size() + sz);
        }
        for (unsigned int i = 0; i < sz; ++i) {
            CXCompileCommand cmd = clang_CompileCommands_getCommand(cmds, i);
            String args;
            CXString str = clang_CompileCommand_getDirectory(cmd);
            const Path compileDir = clang_getCString(str);
            clang_disposeString(str);
            const unsigned int num = clang_CompileCommand_getNumArgs(cmd);
            for (unsigned int j = 0; j < num; ++j) {
                str = clang_CompileCommand_getArg(cmd, j);
                args += clang_getCString(str);
                clang_disposeString(str);
                if (j < num - 1)
                    args += ' ';
            }
            op->indexes.push_back(CompileCommandsOperation::Index { args, compileDir.ensureTrailingSlash(), info.first } );
        }
        clang_CompileCommands_dispose(cmds);
        clang_CompilationDatabase_dispose(db);
    };


    if (op) {
        EventLoop::eventLoop()->callLater(std::bind(&CompileCommandsOperation::work, op));
        return true;
    }
#endif
    return false;
}
}
