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

#include "Source.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <functional>
#include <initializer_list>
#include <set>
#include <utility>
#include <vector>

#include "Location.h"
#include "rct/Process.h"
#include "RTags.h"
#include "Server.h"
#include "Sandbox.h"
#include "rct/Hash.h"
#include "rct/Map.h"

void Source::clear()
{
    fileId = compilerId = buildRootId = compileCommandsFileId = 0;
    includePathHash = 0;
    language = NoLanguage;

    defines.clear();
    includePaths.clear();
    arguments.clear();
}

Path Source::sourceFile() const
{
    return Location::path(fileId);
}

Path Source::buildRoot() const
{
    return Location::path(buildRootId);
}

Path Source::compileCommands() const
{
    return Location::path(compileCommandsFileId);
}

Path Source::compiler() const
{
    return Location::path(compilerId);
}

String Source::toString() const
{
    String ret = String::join(toCommandLine(IncludeCompiler|IncludeSourceFile|IncludeIncludePaths|QuoteDefines|IncludeDefines|IncludeOutputFilename), ' ');
    if (buildRootId)
        ret << " Build: " << buildRoot();
    if (compileCommandsFileId)
        ret << " compile_commands: " << compileCommands();
    return ret;
}

static inline Source::Language guessLanguageFromSourceFile(const Path &sourceFile)
{
    // ### We should support some more of of these really
    // .Case("cl", IK_OpenCL)
    // .Case("cuda", IK_CUDA)
    // .Case("c++", IK_CXX)
    // .Case("objective-c", IK_ObjC)
    // .Case("objective-c++", IK_ObjCXX)
    // .Case("cpp-output", IK_PreprocessedC)
    // .Case("assembler-with-cpp", IK_Asm)
    // .Case("c++-cpp-output", IK_PreprocessedCXX)
    // .Case("objective-c-cpp-output", IK_PreprocessedObjC)
    // .Case("objc-cpp-output", IK_PreprocessedObjC)
    // .Case("objective-c++-cpp-output", IK_PreprocessedObjCXX)
    // .Case("objc++-cpp-output", IK_PreprocessedObjCXX)
    // .Case("c-header", IK_C)
    // .Case("cl-header", IK_OpenCL)
    // .Case("objective-c-header", IK_ObjC)
    // .Case("c++-header", IK_CXX)
    // .Case("objective-c++-header", IK_ObjCXX)

    const char *suffix = sourceFile.extension();
    if (suffix) {
        if (!strcasecmp(suffix, "cpp")) {
            return Source::CPlusPlus;
        } else if (!strcasecmp(suffix, "cc")) {
            return Source::CPlusPlus;
        } else if (!strcmp(suffix, "C")) {
            return Source::CPlusPlus;
        } else if (!strcmp(suffix, "cp")) {
            return Source::CPlusPlus;
        } else if (!strcmp(suffix, "cxx")) {
            return Source::CPlusPlus;
        } else if (!strcmp(suffix, "c++")) {
            return Source::CPlusPlus;
        } else if (!strcmp(suffix, "c")) {
            return Source::C;
        } else if (!strcmp(suffix, "cu")) {
            return Source::C;
        } else if (!strcmp(suffix, "M")) {
            return Source::ObjectiveCPlusPlus;
        } else if (!strcmp(suffix, "mm")) {
            return Source::ObjectiveCPlusPlus;
        } else if (!strcmp(suffix, "m")) {
            return Source::ObjectiveC;
        }
    }
    return Source::NoLanguage;
}


static bool isWrapper(const char *name)
{
    return (!strcmp(name, "gcc-rtags-wrapper.sh") || !strcmp(name, "icecc") || !strcmp(name, "fiskc"));
}

static inline String trim(const char *start, int size)
{
    while (size && isspace(*start)) {
        ++start;
        --size;
    }
    while (size && isspace(start[size - 1])) {
        --size;
    }
    return String(start, size);
}

static inline size_t hashIncludePaths(const List<Source::Include> &includes, const Path &buildRoot, Flags<Server::Option> flags)
{
    size_t hash = 0;
    std::hash<Path> hasher;
    for (const auto &inc : includes) {
        size_t h;
        if (flags & Server::SourceIgnoreIncludePathDifferencesInUsr && inc.path.startsWith("/usr/") && !inc.path.startsWith("/usr/home/")) {
            continue;
        }

        if (!buildRoot.isEmpty() && inc.path.startsWith(buildRoot)) {
            h = hasher(inc.path.mid(buildRoot.size()));
        } else {
            h = hasher(inc.path);
        }
        h += inc.type;
        hash ^= h + 0x9e3779b9 + (h << 6) + (h >> 2);
        // Bit twiddling found here:
        // http://stackoverflow.com/questions/15741615/c-suggestions-about-a-hash-function-for-a-sequence-of-strings-where-the-order
        // apparently from boost.
    }
    return hash;
}

static const char *valueArgs[] = {
    "--param",
    "-G",
    "-I",
    "-MF",
    "-MQ",
    "-MT",
    "-T",
    "-V",
    "-Xanalyzer",
    "-Xassembler",
    "-Xclang",
    "-Xlinker",
    "-Xpreprocessor",
    "-arch",
    "-b",
    "-gcc-toolchain",
    "-imacros",
    "-imultilib",
    "-include",
    "-iprefix",
    "-isysroot",
    "-ivfsoverlay",
    "-iwithprefix",
    "-iwithprefixbefore",
    "-o",
    "-target",
    "-x"
};

static const char *blacklist[] = {
    "--driver-mode=",
    "--param",
    "-M",
    "-MD",
    "-MF",
    "-MG",
    "-MM",
    "-MMD",
    "-MP",
    "-MQ",
    "-MT",
    "-Og",
    "-Wa,--32",
    "-Wa,--64",
    "-Wl,--incremental-full",
    "-Wl,--incremental-patch,1",
    "-Wl,--no-incremental",
    "-fbuild-session-file=",
    "-fbuild-session-timestamp=",
    "-fembed-bitcode",
    "-fembed-bitcode-marker",
    "-fmodules-validate-once-per-build-session",
    "-fno-delete-null-pointer-checks",
    "-fno-use-linker-plugin",
    "-fno-var-tracking",
    "-fno-var-tracking-assignments",
    "-fno-enforce-eh-specs",
    "-fvar-tracking",
    "-fvar-tracking-assignments",
    "-fvar-tracking-assignments-toggle",
    "-gcc-toolchain",
    "-march=",
    "-masm=",
    "-mcpu=",
    "-mfpmath=",
    "-mtune=",
    "-s"
};

static int compare(const void *s1, const void *s2)
{
    const char *key = static_cast<const char*>(s1);
    const char * const * arg = static_cast<const char *const *>(s2);
    return strcmp(key, *arg);
}

static inline bool hasValue(const String &arg)
{
    if (bsearch(arg.constData(), valueArgs,
                sizeof(valueArgs) / sizeof(valueArgs[0]),
                sizeof(valueArgs[0]), compare))
        return true;

    if (const Server *server = Server::instance()) {
        for (const String &blockedArg : server->options().blockedArguments) {
            if (blockedArg.endsWith('=') && blockedArg.startsWith(arg)) {
                return true;
            }
        }
    }

    return false;
}

static inline bool isBlacklisted(const String &arg)
{
    const char *cstr;
    const size_t idx = arg.indexOf('=');
    String copy;
    if (idx == String::npos) {
        cstr = arg.c_str();
    } else {
        copy = arg.left(idx + 1);
        cstr = copy.c_str();
    }

    return bsearch(cstr, blacklist,
                   sizeof(blacklist) / sizeof(blacklist[0]),
                   sizeof(blacklist[0]), compare);
}

static inline String unquote(const String &arg)
{
    if (arg.size() >= 4 && arg.startsWith("\\\"") && arg.endsWith("\\\"")) {
        return arg.mid(1, arg.size() - 3) + '\"';
    } else if (arg.size() >= 2 && arg.startsWith('"') && arg.endsWith('"')) {
        return arg.mid(1, arg.size() - 2);
    } else if (arg.size() >= 2 && arg.startsWith('\'') && arg.endsWith('\'')) {
        return arg.mid(1, arg.size() - 2);
    }
    return arg;
}

static inline bool isCompiler(const Path &fullPath, const List<String> &environment)
{
    if (Server::instance()->options().compilerWrappers.contains(fullPath.fileName()))
        return true;
    if (String(fullPath.fileName()).contains("emacs", String::CaseInsensitive))
        return false;
    if (access(fullPath.constData(), R_OK | X_OK)) // can't execute it
        return false;

    static Hash<Path, bool> sCache;

    bool ok;
    bool ret = sCache.value(fullPath, false, &ok);
    if (ok)
        return ret;

    char path[PATH_MAX];
    strcpy(path, "/tmp/rtags-compiler-check-XXXXXX");
    const int fd = mkstemp(path);
    if (fd == -1) {
        error("Failed to make temporary file errno: %d", errno);
        return false;
    }

    const char *contents = "int foo() { return 0; }";
    const ssize_t len = strlen(contents);
    if (write(fd, contents, len) != len) {
        error("Failed to write to temporary file errno: %d", errno);
        close(fd);
        return false;
    }

    close(fd);

    Path out = path;
    out += ".out";
    Process proc;
    List<String> args;
    args << "-x" << "c" << "-c" << path << "-o" << out;
    proc.exec(fullPath, args, environment);
    if (proc.returnCode() != 0) {
        warning() << "Failed to compile" << fullPath << args << "\nwith ENV:\n" << environment
                  << "\nstderr:\n" << proc.readAllStdErr()
                  << "\nstdout:\n" << proc.readAllStdOut();
    }
    assert(proc.isFinished());
    sCache[fullPath] = !proc.returnCode();
    unlink(path);
    unlink(out.constData());
    return !proc.returnCode();
}

static std::pair<Path, bool> resolveCompiler(const Path &unresolved,
                                             const Path &cwd,
                                             const List<String> &environment,
                                             const List<Path> &pathEnvironment,
                                             SourceCache *cache)
{
    std::pair<Path, bool> dummy;
    auto &compiler = cache ? cache->compilerCache[unresolved] : dummy;
    if (compiler.first.isEmpty()) {
        bool wrapper = false;
        // error() << "Coming in with" << unresolved << cwd << pathEnvironment;
        Path resolve;
        Path file;
        if (unresolved.isAbsolute()) {
            resolve = unresolved;
        } else if (unresolved.contains('/')) {
            assert(cwd.endsWith('/'));
            resolve = cwd + unresolved;
        } else {
            file = unresolved;
        }

        if (!resolve.isEmpty()) {
            const Path resolved = resolve.resolved();
            if (isWrapper(resolved.fileName())) {
                wrapper = true;
            }
            compiler.first = resolve;
        }

        if (compiler.first.isEmpty()) {
            for (const Path &path : pathEnvironment) {
                bool ok;
                const Path p = Path::resolved(file, Path::RealPath, path, &ok);
                if (ok) {
                    if (!isWrapper(p.fileName()) && !access(p.constData(), R_OK | X_OK)) {
                        debug() << "Found compiler" << p << "for" << unresolved;
                        compiler.first = Path::resolved(file, Path::MakeAbsolute, path);
                        break;
                    }
                }
            }
        }
        if (!compiler.first.isFile()) {
            compiler.first.clear();
        } else {
            if (compiler.first.contains(".."))
                compiler.first.canonicalize();
            compiler.second = wrapper || isCompiler(compiler.first, environment);
        }
    }

    return compiler;
}

static List<String> splitCommandLine(const String &cmdLine)
{
    List<String> split;
    char quote = '\0';
    const char *cur = cmdLine.data();
    const char *prev = cur;
    int size = cmdLine.size();
    int escape = 0;
    while (size > 0) {
        switch (*cur) {
        case '"':
        case '\'':
            if (escape % 2 == 0) {
                if (quote == '\0') {
                    quote = *cur;
                } else if (*cur == quote) {
                    quote = '\0';
                }
            }
            escape = 0;
            break;
        case '\\':
            ++escape;
            break;
        case ' ':
            if (quote == '\0') {
                if (cur > prev)
                    split.append(unquote(trim(prev, cur - prev)));
                prev = cur + 1;
            }
            escape = 0;
            break;
        default:
            escape = 0;
            break;
        }
        --size;
        ++cur;
    }
    if (cur > prev)
        split.append(trim(prev, cur - prev));
    return split;
}

struct Input {
    Path realPath, absolute, unmolested;
    Source::Language language;
};

SourceList Source::parse(const String &cmdLine,
                         const Path &cwd,
                         const List<String> &environment,
                         List<Path> *unresolvedInputLocations,
                         SourceCache *cache)
{
    List<Path> pathEnvironment;
    for (const String &env : environment) {
        if (env.startsWith("PATH=")) {
            pathEnvironment = env.mid(5).split(':', String::SkipEmpty);
            break;
        }
    }
    assert(cwd.endsWith('/'));
    assert(!unresolvedInputLocations || unresolvedInputLocations->isEmpty());
    List<String> split = splitCommandLine(cmdLine);
    if (split.isEmpty())
        return SourceList();

    debug() << "Source::parse (" << cmdLine << ") => " << split << cwd;
    size_t idx = 0;
    if (split.size() > 1 && (split.at(0).endsWith("/ccache") || split.at(0) == "ccache"))
        ++idx;
    if (split.at(idx).endsWith("/fiskc") || split.at(idx) == "fiskc") {
        String compiler;
        split.removeAt(0);
        size_t i=idx;
        while (i < split.size()) {
            String &str = split[i];
            if (str.startsWith("--fisk-compiler")) {
                if (str.contains("=")) {
                    compiler = str.mid(16);
                    split.removeAt(i);
                } else if (i + 1 < split.size()) {
                    compiler = std::move(split[i + 1]);
                    split.remove(i, 2);
                }
            } else if (str.startsWith("--fisk-")) {
                if (str.contains("=")) {
                    split.removeAt(i);
                } else {
                    split.remove(i, 2);
                }
            } else {
                ++i;
            }
        }
        if (!compiler.isEmpty())
            split.insert(idx, compiler);
        debug() << "Postfisk Source::parse (" << split;
    }

    for (size_t i=0; i<split.size(); ++i) {
        if (split.at(i) == "cd" || !resolveCompiler(split.at(i), cwd, environment, pathEnvironment, cache).first.isEmpty()) {
            if (i) {
                split.remove(0, i);
            }
            break;
        }
    }

    if (split.isEmpty()) {
        warning() << "Source::parse No args" << cmdLine;
        return SourceList();
    }

    Path path;
    if (split.front() == "cd" && split.size() > 3 && split.at(2) == "&&") {
        path = Path::resolved(split.at(1), Path::MakeAbsolute, cwd);
        split.erase(split.begin(), split.begin() + 3);
    } else {
        path = cwd;
    }
    if (split.isEmpty()) {
        warning() << "Source::parse No args" << cmdLine;
        return SourceList();
    }

    // expand any response file references
    for (size_t i=0; i<split.size(); ++i) {
        auto arg = split.at(i);
        if (!arg.startsWith('@'))
            continue;
        arg.remove(0, 1);
        Path responseFile = Path::resolved(arg, Path::MakeAbsolute, cwd);
        if (!responseFile.isFile())
            continue;
        auto contents = responseFile.readAll();
        if (contents.isEmpty())
            continue;
        contents.chomp("\r\n\t ");
        List<String> subcommands = splitCommandLine(contents);
        if (!subcommands.isEmpty()) {
            split.removeAt(i);
            split.insert(i, subcommands);
            i += subcommands.size() - 1;
        }
    }

    List<Input> inputs;
    Language language = NoLanguage;
    Flags<Flag> sourceFlags;
    List<String> arguments;
    Set<Define> defines;
    List<Include> includePaths;
    uint32_t buildRootId = 0;
    Path buildRoot;
    Path outputFilename;
    uint32_t compilerId = 0;
    uint64_t includePathHash = 0;
    bool validCompiler = false;

    const int s = split.size();
    String arg;
    Path extraCompiler;
    bool verbose = testLog(LogLevel::Debug);
    for (int i=0; i<s; ++i) {
        arg = split.at(i);
        if (verbose)
            debug() << "parsing argument" << i << arg;
        if (arg.isEmpty())
            continue;
        if ((arg.startsWith('\'') && arg.endsWith('\'')) ||
            (arg.startsWith('"') && arg.endsWith('"')))
            arg = arg.mid(1, arg.size() - 2);
        // ### is this even right?
        if (arg.size() > 1 && arg.startsWith('-')) {
            if (arg == "-E") {
                warning() << "Preprocessing, ignore" << cmdLine;
                return SourceList();
            } else if (arg.startsWith("-x")) {
                String a;
                if (arg.size() == 2) {
                    a = split.value(++i);
                } else {
                    a = arg.mid(2);
                }
                if (a == "c-header") {
                    language = CHeader;
                } else if (a == "c++-header") {
                    language = CPlusPlusHeader;
                } else if (a == "c") {
                    language = C;
                } else if (a == "c++") {
                    language = CPlusPlus;
                } else if (a == "objective-c") {
                    language = ObjectiveC;
                } else if (a == "objective-c++") {
                    language = ObjectiveCPlusPlus;
                } else if (arg.size() > 2) {
                    // intel compiler passes compiler options like SSE this way,
                    // just ignore:
                    // https://software.intel.com/en-us/cpp-compiler-developer-guide-and-reference-x-qx
                    a.clear();
                } else {
                    return SourceList();
                }
                if (!a.isEmpty()) {
                    arguments.append("-x");
                    arguments.append(a);
                }
            } else if (arg.startsWith("-D")) {
                Define define;
                String def, a;
                if (arg.size() == 2) {
                    def = split.value(++i);
                    a = arg + def;
                } else {
                    a = arg;
                    def = arg.mid(2);
                }
                if (!def.isEmpty()) {
                    const int eq = def.indexOf('=');
                    if (eq == -1) {
                        define.define = def;
                        define.flags |= Define::NoValue;
                    } else {
                        define.define = def.left(eq);
                        define.value = def.mid(eq + 1);
                    }
                    debug("Parsing define: [%s] => [%s]%s[%s]", def.constData(),
                          define.define.constData(),
                          define.value.isEmpty() ? "" : "=",
                          define.value.constData());
                    defines.insert(define);
                }
#ifdef OS_Darwin
            } else if (arg == "-arch") {
                // Limit -arch to a single format i368/x86_64. Darwin allows
                // mutliple -arch options to build a combined binary.  However,
                // libclang (the indexer) will fail if it gets more than one; it
                // only allows one 'job', in clang parlance, per invocation. It
                // quietly returns a null CXTranslationUnit and is very
                // difficult to see why indexing failed (ie. debug)
                if (!arguments.contains(arg)) {
                    arguments.append(arg);
                    arguments.append(split.value(++i));
                } else {
                    warning() << "[Source::parse] Removing additional -arch argument(s) to allow indexing.";
                }
#endif
            } else if (arg == "-ObjC++") {
                language = ObjectiveCPlusPlus;
                arguments.append(arg);
            } else if (arg == "-ObjC") {
                language = ObjectiveC;
                arguments.append(arg);
            } else if (arg == "-fno-rtti") {
                sourceFlags |= NoRtti;
                arguments.append(arg);
            } else if (arg == "-m32") {
                sourceFlags |= M32;
                arguments.append(arg);
            } else if (arg == "-m64") {
                sourceFlags |= M64;
                arguments.append(arg);
            } else if (arg == "-frtti") {
                sourceFlags &= ~NoRtti;
                arguments.append(arg);
            } else if (arg.startsWith("-std=")) {
                arguments.append(arg);
                // error() << "Got std" << arg;
                if (arg == "-std=c++0x" || arg == "-std=c++11" || arg == "-std=gnu++0x" || arg == "-std=gnu++11") {
                    if (language == CPlusPlusHeader) {
                        language = CPlusPlus11Header;
                    } else {
                        language = CPlusPlus11;
                    }
                }
            } else if (arg.startsWith("-o")) {
                Path p;
                if (arg.size() > 2) {
                    p = arg.mid(2);
                } else if (i + 1 < s) {
                    p = split.value(++i);
                }
                if (!p.isEmpty()) {
                    bool ok;
                    p = Path::resolved(p, Path::RealPath, path, &ok);
                    // error() << p << ok << split.value(i) << Path::resolved(split.value(i), Path::MakeAbsolute);
                    if (!ok && !p.isAbsolute()) {
                        p.prepend(path); // the object file might not exist
                        p.canonicalize();
                    }
                    buildRoot = RTags::findProjectRoot(p, RTags::BuildRoot, cache);
                    buildRoot.resolve(Path::RealPath, cwd);
                    if (buildRoot.isDir()) {
                        buildRootId = Location::insertFile(buildRoot);
                    } else {
                        buildRoot.clear();
                    }
                }
                outputFilename = std::move(p);
            }
#define DECLARE_INCLUDE_TYPE(type, argument, space)                     \
            else if (arg.startsWith(argument)) {                        \
                const size_t argLen = strlen(argument);                 \
                Path p;                                                 \
                if (arg.size() == argLen) {                             \
                    p = Path::resolved(split.value(++i), Path::MakeAbsolute, path); \
                } else {                                                \
                    p = Path::resolved(arg.mid(argLen), Path::MakeAbsolute, cwd); \
                }                                                       \
                if (testLog(LogLevel::Warning))                         \
                    warning() << "Added include path" << p <<           \
                    "type:" << #type << "for argument" << arg;          \
                includePaths.append(Source::Include(Source::Include::type, p)); \
            }
#include "IncludeTypesInternal.h"
#undef DECLARE_INCLUDE_TYPE
            else {
                arguments.append(arg);
                if (hasValue(arg)) {
                    arguments.append(Path::resolved(split.value(++i), Path::MakeAbsolute, path));
                }
            }
        } else {
            bool add = true;
            Path resolved;
            if (!compilerId) {
                add = false;
                const std::pair<Path, bool> compiler = resolveCompiler(arg, cwd, environment, pathEnvironment, cache);
                if (!compiler.first.isEmpty()) {
                    validCompiler = compiler.second;
                    compilerId = Location::insertFile(compiler.first);
                } else {
                    break;
                }
            } else {
                const Path c = arg;
                resolved = Path::resolved(arg, Path::RealPath, cwd);
                if (arg != "-" && (!resolved.extension() || !resolved.isHeader()) && !resolved.isSource()) {
                    add = false;
                    if (i == 1) {
                        const std::pair<Path, bool> inPath = resolveCompiler(c, cwd, environment, pathEnvironment, cache);
                        if (!inPath.first.isEmpty()) {
                            extraCompiler = inPath.first;
                            if (!validCompiler)
                                validCompiler = inPath.second;
                        }
                    }
                }
            }
            if (add) {
                const Language lang = language != NoLanguage ? language : guessLanguageFromSourceFile(resolved);
                if (lang != NoLanguage) {
                    inputs.append({resolved, Path::resolved(arg, Path::MakeAbsolute, cwd), arg, lang});
                } else {
                    warning() << "Can't figure out language for" << arg;
                }
            }
        }
    }

    if (!validCompiler) {
        warning() << "Source::parse Nothing looks like a compiler" << Location::path(compilerId) << extraCompiler;
        return SourceList();
    }

    if (inputs.isEmpty()) {
        warning() << "Source::parse No file for" << cmdLine;
        return SourceList();
    }

    SourceList ret;
    if (!inputs.isEmpty()) {
        if (!buildRootId) {
            buildRoot = RTags::findProjectRoot(inputs.first().realPath, RTags::BuildRoot, cache);
            if (buildRoot.isDir())
                buildRootId = Location::insertFile(buildRoot);
        }
        Flags<Server::Option> serverFlags = Server::instance() ? Server::instance()->options().options : NullFlags;
        includePathHash = ::hashIncludePaths(includePaths, buildRoot, serverFlags);

        ret.reserve(inputs.size());
        for (const auto& input : inputs) {
            unresolvedInputLocations->append(input.absolute);
            if (input.unmolested == "-")
                continue;
            Source source;
            source.directory = path;
            source.fileId = Location::insertFile(input.realPath);
            source.extraCompiler = extraCompiler;
            source.compilerId = compilerId;
            source.buildRootId = buildRootId;
            source.includePathHash = includePathHash;
            source.flags = sourceFlags;
            source.defines = defines;
            source.includePaths = includePaths;
            source.arguments = arguments;
            source.outputFilename = outputFilename;
            source.language = input.language;
            assert(source.language != NoLanguage);
            ret.emplace_back(std::move(source));
        }
    }
    if (testLog(LogLevel::Warning))
        warning() << "Parsed Source(s) successfully:" << ret;
    return ret;
}
// returns false if at end
static inline bool advance(Set<Source::Define>::const_iterator &it, const Set<Source::Define>::const_iterator end)
{
    while (it != end) {
        if (it->define != "NDEBUG")
            return true;
        ++it;
    }
    return false;
}

static inline bool compareDefinesNoNDEBUG(const Set<Source::Define> &l, const Set<Source::Define> &r)
{
    auto lit = l.begin();
    auto rit = r.begin();
    while (true) {
        if (!advance(lit, l.end())) {
            if (advance(rit, r.end()))
                return false;
            break;
        } else if (!advance(rit, r.end())) {
            return false;
        }

        if (*lit != *rit) {
            return false;
        }
        ++lit;
        ++rit;
    }
    return true;
}

static bool nextArg(List<String>::const_iterator &it,
                    const List<String>::const_iterator end,
                    const Flags<Server::Option> flags)
{
    while (it != end) {
        if (isBlacklisted(*it)) {
            const bool val = hasValue(*it);
            ++it;
            if (val && it != end)
                ++it;
        } else if (!(flags & Server::SeparateDebugAndRelease) && (*it == "-g" || it->startsWith("-O"))) {
            ++it;
        } else if (!(flags & Server::Separate32BitAnd64Bit) && (*it == "-m32" || *it == "-m64")) {
            ++it;
        } else {
            break;
        }
    }
    return it != end;
}

bool Source::compareArguments(const Source &other) const
{
    assert(fileId == other.fileId);

    if  (includePathHash != other.includePathHash) {
        warning() << "includepathhash is different";
        return false;
    }

    const Server *server = Server::instance();
    const Flags<Server::Option> serverFlags = server ? server->options().options : NullFlags;
    if (serverFlags & Server::SeparateDebugAndRelease) {
        if (defines != other.defines) {
            warning() << "defines are different 1";
            return false;
        }
    } else if (!compareDefinesNoNDEBUG(defines, other.defines)) {
        warning() << "defines are different 2";
        return false;
    }

    auto me = arguments.begin();
    const auto myEnd = arguments.end();
    auto him = other.arguments.begin();
    const auto hisEnd = other.arguments.end();

    while (me != him) {
        if (!nextArg(me, myEnd, serverFlags))
            break;
        if (!nextArg(him, hisEnd, serverFlags)) {
            warning() << "Couldn't find arg for him" << *me;
            return false;
        }
        if (*me != *him) {
            warning() << "Args are different" << *me << *him;
            return false;
        }
        ++me;
        ++him;
    }
    if (him == hisEnd) {
        warning() << "Args are the same";
        return true;
    } else if (!nextArg(him, hisEnd, serverFlags)) {
        warning() << "Args are the same";
        return true;
    }
    warning() << "He has an arg that I don't have" << *him;
    return false;
}

List<String> Source::toCommandLine(Flags<CommandLineFlag> f, bool *usedPch) const
{
    if (usedPch)
        *usedPch = false;
    const Server *server = Server::instance();
    if (!server)
        f |= (ExcludeDefaultArguments|ExcludeDefaultDefines|ExcludeDefaultIncludePaths);

    List<String> ret;
    ret.reserve(64);
    if ((f & IncludeCompiler) == IncludeCompiler) {
        ret.append(compiler());
    }
    if (f & IncludeExtraCompiler && !extraCompiler.isEmpty()) {
        ret.append(extraCompiler);
    }

    Map<String, String> config;
    Set<String> remove;
    if (f & IncludeRTagsConfig) {
        config = RTags::rtagsConfig(sourceFile());
        remove = config.value("remove-arguments").split(";").toSet();
    }

    if (!(f & ExcludeDefaultArguments)) {
        assert(server);
        for (const auto &arg : server->options().defaultArguments)
            ret.append(arg);
    }

    for (size_t i=0; i<arguments.size(); ++i) {
        const String &arg = arguments.at(i);
        const bool hasValue = ::hasValue(arg);
        bool skip = false;
        if (f & FilterBlacklist && isBlacklisted(arg)) {
            skip = true;
        }
        if (!skip && remove.contains(arg))
            skip = true;
        if (!skip) {
            ret.append(arg);
            if (hasValue)
                ret.append(arguments.value(++i));
        } else if (hasValue) {
            ++i;
        }
    }

    if (f & IncludeDefines) {
        for (const auto &def : defines)
            ret += def.toString(f);
        if (!(f & ExcludeDefaultDefines)) {
            assert(server);
            for (const auto &def : server->options().defines)
                if (!defines.contains(def))
                    ret += def.toString(f);
        }
    }
    if (f & IncludeIncludePaths) {
        for (const auto &inc : includePaths) {
            switch (inc.type) {
            case Include::Type_None: assert(0 && "Impossible impossibility"); break;
#define DECLARE_INCLUDE_TYPE(type, argument, space)                 \
                case Source::Include::type:                         \
                    if (inc.type == Include::Type_PCH) {            \
                        if (f & PCHEnabled) {                       \
                            if (usedPch)                            \
                                *usedPch = true;                    \
                            ret << argument << (inc.path + ".gch"); \
                        }                                           \
                    } else if (*space) {                            \
                        ret << argument << inc.path;                \
                    } else {                                        \
                        ret << (argument + inc.path);               \
                    }                                               \
                    break;
#include "IncludeTypesInternal.h"
            }
        }
    }
    if (!(f & ExcludeDefaultIncludePaths)) {
        assert(server);
        for (const auto &inc : server->options().includePaths) {
            switch (inc.type) {
            case Include::Type_None: assert(0 && "Impossible impossibility"); break;
#include "IncludeTypesInternal.h"
#undef DECLARE_INCLUDE_TYPE
            }
        }
    }
    if (f & IncludeOutputFilename && !outputFilename.isEmpty()) {
        ret << "-o" << outputFilename;
    }
    if (f & IncludeRTagsConfig) {
        ret << config.value("add-arguments").split(' ');
    }

    if (f & IncludeSourceFile)
        ret.append(sourceFile());

    return ret;
}

bool Source::Include::isPch() const
{
    for (const char *suffix : { ".gch", ".pch" }) {
        const Path p = path + suffix;
        if (p.isFile()) {
            return true;
        }
    }
    return false;
}

void Source::encode(Serializer &s, EncodeMode mode) const
{
    // SBROOT
    // sourceFile, buildRoot, compiler(?), includePaths

    if (mode == EncodeSandbox && !Sandbox::root().isEmpty()) {
        s << Sandbox::encoded(sourceFile()) << fileId << Sandbox::encoded(compiler()) << compilerId
          << Sandbox::encoded(extraCompiler) << Sandbox::encoded(buildRoot()) << buildRootId
          << compileCommands() << compileCommandsFileId
          << static_cast<uint8_t>(language) << flags << defines;

        auto incPaths = includePaths;
        for (auto &inc : incPaths)
            Sandbox::encode(inc.path);

        s << incPaths << Sandbox::encoded(arguments)
          << Sandbox::encoded(directory) << includePathHash;
    } else {
        s << sourceFile() << fileId << compiler() << compilerId
          << extraCompiler << buildRoot() << buildRootId
          << compileCommands() << compileCommandsFileId
          << static_cast<uint8_t>(language) << flags << defines
          << includePaths << arguments << directory << includePathHash;
    }
}

void Source::decode(Deserializer &s, EncodeMode mode)
{
    clear();
    uint8_t lang;
    Path source, compiler, buildRoot, compileCommands;
    s >> source >> fileId >> compiler >> compilerId >> extraCompiler
      >> buildRoot >> buildRootId >> compileCommands >> compileCommandsFileId
      >> lang >> flags >> defines >> includePaths >> arguments
      >> directory >> includePathHash;
    language = static_cast<Language>(lang);

    if (mode == EncodeSandbox && !Sandbox::root().isEmpty()) { // SBROOT
        Sandbox::decode(source);
        Sandbox::decode(buildRoot);
        Sandbox::decode(compileCommands);
        Sandbox::decode(compiler);
        Sandbox::decode(extraCompiler);
        Sandbox::decode(directory);
        for (auto &inc : includePaths)
            Sandbox::decode(inc.path);
        Sandbox::decode(arguments);
    }

    assert(fileId);
    Location::set(source, fileId);
    if (compilerId)
        Location::set(compiler, compilerId);
    if (buildRootId)
        Location::set(buildRoot, buildRootId);
    if (compileCommandsFileId)
        Location::set(compileCommands, compileCommandsFileId);
    language = static_cast<Source::Language>(language);
}
