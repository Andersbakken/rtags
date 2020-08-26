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

#include "RTags.h"

#include <dirent.h>
#include <fnmatch.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <map>
#include <unordered_map>
#if defined(OS_FreeBSD) || defined(OS_DragonFly)
#include <sys/sysctl.h>
#endif
#ifdef OS_Darwin
#include <mach-o/dyld.h>
#endif

#include "IndexDataMessage.h"
#include "LogOutputMessage.h"
#include "QueryMessage.h"
#include "rct/Rct.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include "RTagsVersion.h"
#include "Diagnostic.h"
#include "IndexMessage.h"
#include "Sandbox.h"
#include "clang-c/CXErrorCode.h"
#include "clang-c/Index.h"
#include "rct/Date.h"
#include "rct/Message.h"

namespace RTags {
String versionString()
{
    return String::format<64>("%d.%d.%d", MajorVersion, MinorVersion, DatabaseVersion);
}

String encodeUrlComponent(const String &str)
{
    String new_str = "";
    char c;
    int ic;
    const char* chars = str.c_str();
    char bufHex[10];
    const size_t len = str.size();

    for(size_t i=0; i<len; ++i){
        c = chars[i];
        ic = c;
        // uncomment this if you want to encode spaces with +
        if (c == ' ') {
            new_str += '+';
        } else if (isalnum(c) || c == '-' || c == '_' || c == '.' || c == '~') {
            new_str += c;
        } else {
            sprintf(bufHex,"%X",c);
            if (ic < 16) {
                new_str += "%0";
            } else {
                new_str += "%";
            }
            new_str += bufHex;
        }
    }
    return new_str;
}

String decodeUrlComponent(const String &str)
{
    String ret;
    char ch;
    const size_t len = str.length();

    for (size_t i=0; i < len; ++i) {
        if (str[i] != '%') {
            if (str[i] == '+') {
                ret += ' ';
            } else {
                ret += str[i];
            }
        } else {
            int ii;
            if (sscanf(str.mid(i + 1, 2).c_str(), "%x", &ii) != 1)
                return String();
            ch = static_cast<char>(ii);
            ret += ch;
            i = i + 2;
        }
    }
    return ret;
}

void encodePath(Path &path)
{
    if (Sandbox::encode(path))
        return;

    path = encodeUrlComponent(path);
}

void decodePath(Path &path)
{
    if (Sandbox::decode(path))
        return;

    path = decodeUrlComponent(path);
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

Path findAncestor(const Path& path, const String &fn, Flags<FindAncestorFlag> flags, SourceCache *cache)
{
    Path *cacheResult = nullptr;
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
                for (const auto &entry : it->second) {
                    auto &ref = ret[entry.first];
                    if (ref.isEmpty())
                        ref = entry.second;
                }
                dir = dir.parentDir();
                continue;
            }
        }
        Map<String, String> *cacheEntry = nullptr;
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
                    char *value = nullptr;
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
            { nullptr, Flags<FindAncestorFlag>() }
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
        { nullptr, Flags<FindAncestorFlag>() }
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

size_t findOffset(int line, int col, const String &contents, size_t offset)
{
    // ### this does not handle multibyte
    String ret;
    unsigned int l = line;
    if (!l)
        return String::npos;
    const char *ch = contents.constData() + offset;
    while (--l) {
        ch = strchr(ch, '\n');
        if (!ch)
            return String::npos;
        ++ch;
    }
    return (ch - contents.constData()) + col - 1;
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

String cursorToString(CXCursor cursor, const Flags<CursorToStringFlags> flags)
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
        const String usr = RTags::usr(cursor);
        if (!usr.isEmpty()) {
            ret += " " + usr;
        }
    }

    if (flags & IncludeSpecializedUsr) {
        const CXCursor general = clang_getSpecializedCursorTemplate(cursor);
        if (!clang_Cursor_isNull(general)) {
            const String usr = RTags::usr(general);
            if (!usr.isEmpty()) {
                ret += " " + usr;
            }
        }
    }

    if (flags & IncludeStructSizeof && Symbol::isClass(kind)) {
        const long long size = clang_Type_getSizeOf(clang_getCursorType(cursor));
        switch (size) {
        case CXTypeLayoutError_Invalid:
            // ret += " (sizeof: invalid)";
            break;
        case CXTypeLayoutError_Incomplete:
            ret += " (sizeof: incomplete)";
            break;
        case CXTypeLayoutError_Dependent:
            ret += " (sizeof: dependent)";
            break;
        default:
            ret += String::format(" (sizeof: %lld)", size);
            break;
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
            if (flags & RealPathCursorPath) {
                char buf[PATH_MAX];
                if (realpath(data, buf)) {
                    ret += buf;
                } else {
                    ret += data;
                }
            } else {
                ret += data;
            }
            ret += ':';
            ret += String::number(line);
            ret += ':';
            ret += String::number(col);
        }
        clang_disposeString(file);
    }
    return ret;
}

std::shared_ptr<TranslationUnit> TranslationUnit::load(const Path &path)
{
    auto ret = std::make_shared<TranslationUnit>();
    ret->index = clang_createIndex(0, false);
#if CINDEX_VERSION_MINOR >= 23
    CXErrorCode error = clang_createTranslationUnit2(ret->index, path.constData(), &ret->unit);
    if (error != CXError_Success) {
        ret.reset();
        ::error() << "Failed to load" << path << error << path.exists();
    }
#else
    ret->unit = clang_createTranslationUnit(ret->index, path.constData());
    if (!ret->unit) {
        ret.reset();
        ::error() << "Failed to load" << path << path.exists();
    }
#endif
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
    List<const char*> clangArgs(args.size() + 2, nullptr);

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
    const int ret = clang_reparseTranslationUnit(unit, unsavedCount, unsaved, clang_defaultReparseOptions(unit));
    if (ret) {
        clang_disposeTranslationUnit(unit);
        unit = nullptr;
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
    while (type.kind == CXType_Pointer || type.kind == CXType_LValueReference || type.kind == CXType_RValueReference)
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

static inline Diagnostic::Flag convertDiagnosticType(CXDiagnosticSeverity sev)
{
    Diagnostic::Flag type = Diagnostic::None;
    switch (sev) {
    case CXDiagnostic_Warning:
        type = Diagnostic::Warning;
        break;
    case CXDiagnostic_Error:
    case CXDiagnostic_Fatal:
        type = Diagnostic::Error;
        break;
    case CXDiagnostic_Note:
        type = Diagnostic::Note;
        break;
    default:
        break;
    }
    return type;
}

void DiagnosticsProvider::diagnose()
{
    const uint32_t sourceFile = sourceFileId();

    std::function<void(uint32_t, CXDiagnostic, Diagnostics &, Flags<Diagnostic::Flag>)> process;
    process = [&](uint32_t u, CXDiagnostic d, Diagnostics &m, Flags<Diagnostic::Flag> flags) {
        flags |= convertDiagnosticType(clang_getDiagnosticSeverity(d));
        if ((flags & Diagnostic::Type_Mask) != Diagnostic::None) {
            const CXSourceLocation diagLoc = clang_getDiagnosticLocation(d);
            Location location = createLocation(diagLoc, nullptr);
            const uint32_t fileId = location.fileId();

            int length = -1;
            Map<Location, int> ranges;

            const unsigned int rangeCount = clang_getDiagnosticNumRanges(d);
            bool first = true;
            for (unsigned int rangePos = 0; rangePos < rangeCount; ++rangePos) {
                const CXSourceRange range = clang_getDiagnosticRange(d, rangePos);
                const CXSourceLocation start = clang_getRangeStart(range);
                const CXSourceLocation end = clang_getRangeEnd(range);

                unsigned int startOffset, endOffset;
                clang_getSpellingLocation(start, nullptr, nullptr, nullptr, &startOffset);
                clang_getSpellingLocation(end, nullptr, nullptr, nullptr, &endOffset);
                if (startOffset && endOffset) {
                    unsigned int line, column;
                    clang_getSpellingLocation(start, nullptr, &line, &column, nullptr);
                    const Location l(fileId, line, column);
                    if (first) {
                        first = false;
                        location = l;
                        length = endOffset - startOffset;
                    } else {
                        ranges[l] = endOffset - startOffset;
                    }
                }
            }

            String message;
            if (flags & Diagnostic::DisplayCategory)
                message << RTags::eatString(clang_getDiagnosticCategoryText(d));
            if (!message.isEmpty())
                message << ": ";
            message << RTags::eatString(clang_getDiagnosticSpelling(d));

            const String option = RTags::eatString(clang_getDiagnosticOption(d, nullptr));
            if (!option.isEmpty()) {
                message << ": " << option;
            }

            Diagnostic &diagnostic = m[location];
            diagnostic.flags = flags;
            diagnostic.message = std::move(message);
            diagnostic.ranges = std::move(ranges);
            diagnostic.length = length;
            diagnostic.sourceFileId = sourceFile;

            if (CXDiagnosticSet children = clang_getChildDiagnostics(d)) {
                const unsigned int childCount = clang_getNumDiagnosticsInSet(children);
                for (unsigned j=0; j<childCount; ++j) {
                    process(u, clang_getDiagnosticInSet(children, j), diagnostic.children, NullFlags);
                }
                clang_disposeDiagnosticSet(children);
            }
        }
    };

    IndexDataMessage &indexData = indexDataMessage();

    const size_t numUnits = unitCount();
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 21)
    List<Diagnostics> skipped;
    if (numUnits > 1)
        skipped.resize(numUnits);
#endif

    for (size_t u=0; u<numUnits; ++u) {
        List<String> compilationErrors;
        const size_t diagCount = diagnosticCount(u);

        for (size_t j=0; j<diagCount; ++j) {
            CXDiagnostic diag = diagnostic(u, j);
            const CXSourceLocation diagLoc = clang_getDiagnosticLocation(diag);
            const uint32_t fileId = createLocation(diagLoc, nullptr).fileId();
            if (!fileId) {
                clang_disposeDiagnostic(diag);
                // error() << "Couldn't get location for diagnostics" << clang_getCursor(tu, diagLoc) << fileId << mSource.fileId
                //         << clang_getDiagnosticSeverity(diagnostic);
                continue;
            }
            // error() << "Got a dude" << clang_getCursor(tu, diagLoc) << fileId << mSource.fileId
            //         << sev << CXDiagnostic_Error;

            assert(fileId);
            Flags<IndexDataMessage::FileFlag> &fileFlags = indexData.files()[fileId];
            {
                Flags<Diagnostic::Flag> f = Diagnostic::DisplayCategory;
                if (!(fileFlags & IndexDataMessage::Visited)) {
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 21)
                    CXCursor cursor = cursorAt(u, diagLoc);
                    bool found = false;
                    do {
                        if (clang_Cursor_getNumTemplateArguments(cursor) != -1) {
                            found = true;
                            break;
                        }
                        cursor = clang_getCursorSemanticParent(cursor);
                    } while (!clang_isInvalid(clang_getCursorKind(cursor)));

                    if (!found) {
                        // error() << "Ditching diagnostic since we didn't index this file and it doesn't appear to be in a template function"
                        //         << createLocation(diagLoc) << cursorAt(u, diagLoc) << "for" << Location::path(sourceFileId())
                        //         << RTags::eatString(clang_getDiagnosticSpelling(diag));
                        clang_disposeDiagnostic(diag);
                        continue;
                    }
#else
                    continue;
#endif
                }
                process(u, diag, indexData.diagnostics(), f);
            }
            // logDirect(RTags::DiagnosticsLevel, message.constData());

            const unsigned int fixItCount = clang_getDiagnosticNumFixIts(diag);
            for (unsigned int f=0; f<fixItCount; ++f) {
                CXSourceRange range;
                const CXStringScope stringScope = clang_getDiagnosticFixIt(diag, f, &range);
                CXSourceLocation start = clang_getRangeStart(range);

                unsigned int line, column;
                CXFile file;
                clang_getSpellingLocation(start, &file, &line, &column, nullptr);
                if (!file)
                    continue;
                CXStringScope fileName(clang_getFileName(file));

                const Location loc = createLocation(clang_getCString(fileName), line, column);
                if (indexData.files().value(loc.fileId()) & IndexDataMessage::Visited) {
                    unsigned int startOffset, endOffset;
                    CXSourceLocation end = clang_getRangeEnd(range);
                    clang_getSpellingLocation(start, nullptr, nullptr, nullptr, &startOffset);
                    clang_getSpellingLocation(end, nullptr, nullptr, nullptr, &endOffset);
                    const char *string = clang_getCString(stringScope);
                    assert(string);
                    if (!*string) {
                        error("Fixit for %s Remove %d character%s",
                              loc.toString().constData(), endOffset - startOffset,
                              endOffset - startOffset > 1 ? "s" : "");
                    } else if (endOffset == startOffset) {
                        error("Fixit for %s Insert \"%s\"",
                              loc.toString().constData(), string);
                    } else {
                        error("Fixit for %s Replace %d character%s with \"%s\"",
                              loc.toString().constData(), endOffset - startOffset,
                              endOffset - startOffset > 1 ? "s" : "", string);
                    }
                    Diagnostic &entry = indexData.diagnostics()[Location(loc.fileId(), line, column)];
                    entry.flags = Diagnostic::Fixit;
                    entry.sourceFileId = sourceFile;
                    if (entry.message.isEmpty()) {
                        entry.message = String::format<64>("did you mean '%s'?", string);
                    }
                    entry.length = endOffset - startOffset;
                    indexData.fixIts()[loc.fileId()].insert(FixIt(line, column, endOffset - startOffset, string));
                }
            }
            clang_disposeDiagnostic(diag);
        }

#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 21)
        Diagnostics &diags = numUnits > 1 ? skipped[u] : indexData.diagnostics();
        for (const auto &it : indexData.files()) {
            if (it.second & IndexDataMessage::Visited) {
                const Location loc(it.first, 0, 0);
                const Path path = loc.path();
                CXFile file = getFile(u, path.constData());
                if (file) {
                    CXSourceRangeList *s = clang_getSkippedRanges(unit(u), file);
                    if (s) {
                        const unsigned int count = s->count;
                        for (unsigned int j=0; j<count; ++j) {
                            CXSourceLocation start = clang_getRangeStart(s->ranges[j]);

                            unsigned int line, column, startOffset, endOffset;
                            clang_getSpellingLocation(start, nullptr, &line, &column, &startOffset);
                            Diagnostic &entry = diags[Location(loc.fileId(), line, column)];
                            entry.sourceFileId = sourceFile;
                            CXSourceLocation end = clang_getRangeEnd(s->ranges[j]);
                            clang_getSpellingLocation(end, nullptr, nullptr, nullptr, &endOffset);
                            entry.flags = Diagnostic::Skipped;
                            entry.length = endOffset - startOffset;
                            // error() << line << column << startOffset << endOffset;
                        }

                        clang_disposeSourceRangeList(s);
                    }
                }
            }
        }
#endif
    }
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 21)
    if (numUnits > 1) {
        Diagnostics &target = indexData.diagnostics();
        for (auto it = skipped[0].begin(); it != skipped[0].end(); ++it) {
            bool ok = true;
            for (size_t u=1; u<numUnits; ++u) {
                if (skipped[u].value(it->first) != it->second) {
                    ok = false;
                    break;
                }
            }
            if (ok) {
                Diagnostic &diag = target[it->first];
                if (diag.type() == Diagnostic::None)
                    diag = std::move(it->second);
            }
        }
    }
#endif



#if 0
    for (const auto &it : indexData.files()) {
        if (it.second & IndexDataMessage::Visited) {
            const Location loc(it.first, 0, 0);
            const Map<Location, Diagnostic>::const_iterator x = indexData.diagnostics().lower_bound(loc);
            if (x == indexData.diagnostics().end() || x->first.fileId() != it.first) {
                indexData.diagnostics()[loc] = Diagnostic();
            }
        }
    }
#endif
}

Location DiagnosticsProvider::createLocation(const CXCursor &cursor, CXCursorKind kind, bool *blocked, unsigned *offset)
{
    if (kind == CXCursor_FirstInvalid)
        kind = clang_getCursorKind(cursor);
    CXSourceLocation location;
    if (clang_isStatement(kind)) {
        location = clang_getCursorLocation(cursor);
    } else {
        CXSourceRange range = clang_Cursor_getSpellingNameRange(cursor, 0, 0);
        location = clang_getRangeStart(range);
    }
    if (!location)
        return Location();
    return createLocation(location, blocked, offset);
}

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
    CXChildVisitResult result;
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
    return u->result;
}

CXCursor findChild(CXCursor parent, CXCursorKind kind, CXChildVisitResult mode)
{
    FindChildVisitor u = { mode, kind, String(), clang_getNullCursor() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findChildVisitor, &u);
    return u.cursor;
}

CXCursor findChild(CXCursor parent, const String &name, CXChildVisitResult mode)
{
    FindChildVisitor u = { mode, CXCursor_FirstInvalid, name, clang_getNullCursor() };
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
    return eatString(clang_getTypeSpelling(type));
}

#define OUTPUT_LITERAL(string)                  \
    {                                           \
        const char literal[] = string;          \
        output(literal, sizeof(literal) - 1);   \
    }
class ElispFormatter : public Value::Formatter
{
public:
    virtual void format(const Value &value, std::function<void(const char *, size_t)> output) const override
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

struct CursorArgumentsVisitor {
    int numArgs;
    List<CXCursor> *args;
};
static CXChildVisitResult cursorArgumentsVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    if (clang_getCursorKind(cursor) == CXCursor_ParmDecl) {
        CursorArgumentsVisitor *u = static_cast<CursorArgumentsVisitor*>(data);
        ++u->numArgs;
        if (u->args)
            u->args->push_back(cursor);
    }
    return CXChildVisit_Continue;
}
int cursorArguments(const CXCursor &cursor, List<CXCursor> *args)
{
    int numArgs = 0;
    // A workaround for the following issues:
    // + clang_Cursor_getNumArguments() returns -1 with FunctionTemplate
    // + clang_Cursor_getArgument() doesn't work with FunctionTemplate
    //
    if (clang_getCursorKind(cursor) == CXCursor_FunctionTemplate) {
        CursorArgumentsVisitor u = {0, args};
        clang_visitChildren(cursor, cursorArgumentsVisitor, &u);
        numArgs = u.numArgs;
    } else {
        numArgs = clang_Cursor_getNumArguments(cursor);
        if (numArgs > 0 && args) {
            args->resize(numArgs);
            for (int i = 0; i < numArgs; i++) {
                (*args)[i] = clang_Cursor_getArgument(cursor, i);
            }
        }
    }
    return numArgs;
}

String usr(const CXCursor &cursor)
{
    String str = RTags::eatString(clang_getCursorUSR(clang_getCanonicalCursor(cursor)));
    size_t idx = 0;
    while (true) {
        idx = str.indexOf("<", idx);
        if (idx == String::npos)
            break;
        if (idx > 9 && !strncmp(str.c_str() + idx - 8, "operator", 8) && !std::isalnum(str[idx - 9]) && str[idx - 9] != '_') {
            idx += 2;
            continue;
        }
        size_t templateIdx = 0;
        size_t start = ++idx;
        while (idx < str.size()) {
            if (str[idx] == '<') {
                start = ++idx;
            } else if (str[idx] == ',') {
                assert(str[idx + 1] == ' ');
                const String replacement = String::format("T%zu", templateIdx++);
                const ssize_t diff = replacement.size() - (idx - start);
                str.replace(start, idx - start, replacement);
                idx += diff + 2;
                start = idx;
            } else if (str[idx] == '>') {
                str.replace(start, idx - start, String::format("T%zu", templateIdx++));
                break;
            } else {
                ++idx;
            }
        }
    }
    return str;
}
}
