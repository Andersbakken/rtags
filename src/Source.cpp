#include "Source.h"
#include "Location.h"
#include "RTags.h"
#include <rct/EventLoop.h>
#include "Server.h"

void Source::clear()
{
    fileId = compilerId = buildRootId = 0;
    includePathHash = 0;
    language = NoLanguage;
    parsed = 0;

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

Path Source::compiler() const
{
    return Location::path(compilerId);
}

String Source::toString() const
{
    String ret = String::join(toCommandLine(IncludeCompiler|IncludeSourceFile|IncludeIncludepaths|QuoteDefines|IncludeDefines), ' ');
    if (buildRootId)
        ret << " Build: " << buildRoot();
    if (parsed)
        ret << " Parsed: " << String::formatTime(parsed / 1000, String::DateTime);
    return ret;
}

static inline Source::Language guessLanguageFromCompiler(const Path &fullPath) // ### not threadsafe
{
    assert(EventLoop::isMainThread());

    static const List<std::pair<RegExp, Source::Language> > &extraCompilers = Server::instance()->options().extraCompilers;
    for (const auto &pair : extraCompilers) {
        if (pair.first.indexIn(fullPath) != -1)
            return pair.second;
    }

    String compiler = fullPath.fileName();

    String c;
    int dash = compiler.lastIndexOf('-');
    if (dash >= 0) {
        c = String(compiler.constData() + dash + 1, compiler.size() - dash - 1);
    } else {
        c = String(compiler.constData(), compiler.size());
    }

    if (c.size() != compiler.size()) {
        bool isVersion = true;
        for (int i=0; i<c.size(); ++i) {
            if ((c.at(i) < '0' || c.at(i) > '9') && c.at(i) != '.') {
#ifdef OS_CYGWIN
                // eat 'exe' if it exists
                if (c.mid(i) == "exe")
                    goto cont;
#endif
                isVersion = false;
                break;
            }
        }
#ifdef OS_CYGWIN
  cont:
#endif
        if (isVersion) {
            dash = compiler.lastIndexOf('-', dash - 1);
            if (dash >= 0) {
                c = compiler.mid(dash + 1, compiler.size() - c.size() - 2 - dash);
            } else {
                c = compiler.left(dash);
            }
        }
    }

    Source::Language lang = Source::NoLanguage;
    if (c.startsWith("g++") || c.startsWith("c++") || c.startsWith("clang++")) {
        lang = Source::CPlusPlus;
    } else if (c.startsWith("gcc") || c.startsWith("cc") || c.startsWith("clang")) {
        lang = Source::C;
    }
    return lang;
}

static inline Source::Language guessLanguageFromSourceFile(const Path &sourceFile,
                                                           Source::Language defaultLanguage)
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
        } else if (!strcmp(suffix, "M")) {
            return Source::ObjectiveCPlusPlus;
        } else if (!strcmp(suffix, "mm")) {
            return Source::ObjectiveCPlusPlus;
        } else if (!strcmp(suffix, "m")) {
            return Source::ObjectiveC;
        }
    }
    return defaultLanguage;
}

static inline void eatAutoTools(List<String> &args)
{
    List<String> copy = args;
    for (int i=0; i<args.size(); ++i) {
        const String &arg = args.at(i);
        if (arg.endsWith("cc") || arg.endsWith("g++") || arg.endsWith("c++") || arg == "cd") {
            if (i) {
                args.erase(args.begin(), args.begin() + i);
                if (testLog(Debug)) {
                    debug() << "ate something " << copy;
                    debug() << "now we have " << args;
                }
            }
            break;
        }
    }
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

static inline size_t hashIncludePaths(const List<Source::Include> &includes, const Path &buildRoot)
{
    size_t hash = 0;
    std::hash<Path> hasher;
    for (const auto &inc : includes) {
        size_t h;
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

static inline void addIncludeArg(List<Source::Include> &includePaths,
                                 List<String> &arguments,
                                 Source::Include::Type type,
                                 int argLen,
                                 const List<String> &args,
                                 int &idx,
                                 const Path &cwd)
{
    const String &arg = args.at(idx);
    Path path;
    if (arg.size() == argLen) {
        path = Path::resolved(args.value(++idx), Path::MakeAbsolute, cwd);
        if (type == Source::Include::Type_None) {
            arguments.append(arg);
            arguments.append(path);
        }
    } else {
        path = Path::resolved(arg.mid(argLen), Path::MakeAbsolute, cwd);
        if (type == Source::Include::Type_None) {
            arguments.append(arg.left(argLen) + path);
        }
    }
    if (type != Source::Include::Type_None) {
        includePaths.append(Source::Include(type, path));
    }
}


static const char* valueArgs[] = {
    "-I",
    "-o",
    "-x",
    "-target",
    "--param",
    "-imacros",
    "-iprefix",
    "-iwithprefix",
    "-iwithprefixbefore",
    "-imultilib",
    "-isysroot",
    "-Xpreprocessor",
    "-Xassembler",
    "-Xlinker",
    "-Xclang",
    "-Xanalyzer",
    "-T",
    "-V",
    "-b",
    "-G",
    "-arch",
    "-MF",
    "-MT",
    "-MQ",
    0
};

static const char *blacklist[] = {
    "-M",
    "-MM",
    "-MG",
    "-MP",
    "-MD",
    "-MMD",
    "-MF",
    "-MT",
    "-MQ",
    0
};

static inline bool hasValue(const String &arg)
{
    for (int i = 0; valueArgs[i]; ++i) {
        if (arg == valueArgs[i])
            return true;
    }

    if (Server *server = Server::instance()) {
        const Set<String> &blockedArguments = server->options().blockedArguments;
        for (const String &blockedArg : blockedArguments) {
            if (blockedArg.endsWith('=') && blockedArg.startsWith(arg)) {
                return true;
            }
        }
    }

    return false;
}

static inline bool isBlacklisted(const String& arg)
{
    for (int i = 0; blacklist[i]; ++i) {
        if (arg == blacklist[i])
            return true;
    }
    return false;
}

static inline String unquote(const String& arg)
{
    if (arg.size() >= 4 && arg.startsWith("\\\"") && arg.endsWith("\\\"")) {
        return arg.mid(1, arg.size() - 3) + '\"';
    } else if (arg.size() >= 2 && arg.startsWith('"') && arg.endsWith('"')) {
        return arg.mid(1, arg.size() - 2);
    }
    return arg;
}

List<Source> Source::parse(const String &cmdLine, const Path &base, unsigned int flags,
                           List<Path> *unresolvedInputLocations)
{
    String args = cmdLine;
    char quote = '\0';
    List<String> split;
    List<std::pair<uint32_t, Path> > inputs;
    {
        char *cur = args.data();
        char *prev = cur;
        // ### handle escaped quotes?
        int size = args.size();
        while (size > 0) {
            switch (*cur) {
            case '"':
            case '\'':
                if (quote == '\0') {
                    quote = *cur;
                } else if (*cur == quote) {
                    quote = '\0';
                }
                break;
            case ' ':
                if (quote == '\0') {
                    if (cur > prev)
                        split.append(trim(prev, cur - prev));
                    prev = cur + 1;
                }
                break;
            default:
                break;
            }
            --size;
            ++cur;
        }
        if (cur > prev)
            split.append(trim(prev, cur - prev));
    }
    debug() << "Source::parse (" << args << ") => " << split << base;

    eatAutoTools(split);

    if (split.isEmpty()) {
        warning() << "Source::parse No args" << cmdLine;
        return List<Source>();
    }

    Path path;
    if (split.front() == "cd" && split.size() > 3 && split.at(2) == "&&") {
        path = Path::resolved(split.at(1), Path::MakeAbsolute, base);
        split.erase(split.begin(), split.begin() + 3);
    } else {
        path = base;
    }
    if (split.isEmpty()) {
        warning() << "Source::parse No args" << cmdLine;
        return List<Source>();
    }

    if (split.first().endsWith("rtags-gcc-prefix.sh")) {
        if (split.size() == 1) {
            warning() << "Source::parse No args" << cmdLine;
            return List<Source>();
        }
        split.removeAt(0);
    }

    Language language = guessLanguageFromCompiler(split.front());
    bool hasDashX = false;
    uint32_t sourceFlags = 0;
    List<String> arguments;
    Set<Define> defines;
    List<Include> includePaths;
    int32_t sysRootIndex = -1;
    uint32_t buildRootId = 0;
    Path buildRoot;
    uint32_t compilerId = 0;
    uint64_t includePathHash = 0;

    const int s = split.size();
    bool seenCompiler = false;
    String arg;
    for (int i=0; i<s; ++i) {
        arg = split.at(i);
        if (arg.isEmpty())
            continue;
        if ((arg.startsWith('\'') && arg.endsWith('\'')) ||
            (arg.startsWith('"') && arg.endsWith('"')))
            arg = arg.mid(1, arg.size() - 2);
        // ### is this even right?
        if (arg.startsWith('-')) {
            if (arg == "-E") {
                warning() << "Preprocessing, ignore" << cmdLine;
                return List<Source>();
            } else if (arg == "-x") {
                const String a = split.value(++i);
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
                } else {
                    return List<Source>();
                }
                hasDashX = true;
                arguments.append("-x");
                arguments.append(a);
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
                    } else {
                        define.define = def.left(eq);
                        define.value = (flags & Escape ? unquote(def.mid(eq + 1)) : def.mid(eq + 1));
                    }
                    debug("Parsing define: [%s] => [%s]%s[%s]", def.constData(),
                          define.define.constData(),
                          define.value.isEmpty() ? "" : "=",
                          define.value.constData());
                    defines.insert(define);
                }
            } else if (arg.startsWith("-I")) {
                addIncludeArg(includePaths, arguments, Source::Include::Type_Include, 2, split, i, path);
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

            // Framework includes
            } else if (arg.startsWith("-F")) {
                addIncludeArg(includePaths, arguments, Source::Include::Type_Framework, 2, split, i, path);
            } else if (arg.startsWith("-iframework")) {
                addIncludeArg(includePaths, arguments, Source::Include::Type_SystemFramework, 11, split, i, path);
#endif
            } else if (arg.startsWith("-include")) {
                addIncludeArg(includePaths, arguments, Source::Include::Type_None, 8, split, i, path);
            } else if (arg.startsWith("-isystem")) {
                addIncludeArg(includePaths, arguments, Source::Include::Type_System, 8, split, i, path);
            } else if (arg.startsWith("-iquote")) {
                addIncludeArg(includePaths, arguments, Source::Include::Type_None, 7, split, i, path);
            } else if (arg.startsWith("-cxx-isystem")) {
                addIncludeArg(includePaths, arguments, Source::Include::Type_System, 12, split, i, path);
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
            } else if (arg.startsWith("-isysroot")) {
                arguments.append(arg);
                if (i + 1 < s) {
                    sysRootIndex = arguments.size();
                    Path root = split.value(++i);
                    root.resolve();
                    arguments.append(root);
                }
            } else if (arg == "-o") {
                if (i + 1 < s) {
                    bool ok;
                    Path p = Path::resolved(split.value(++i), Path::RealPath, path, &ok);
                    // error() << p << ok << split.value(i) << Path::resolved(split.value(i), Path::MakeAbsolute);
                    if (!ok && !p.isAbsolute()) {
                        p.prepend(path); // the object file might not exist
                        p.canonicalize();
                    }
                    buildRoot = RTags::findProjectRoot(p, RTags::BuildRoot);
                    if (buildRoot.isDir()) {
                        buildRoot.resolve(Path::RealPath);
                        buildRootId = Location::insertFile(buildRoot);
                    } else {
                        buildRoot.clear();
                    }
                }
            } else {
                arguments.append(arg);
                if (hasValue(arg)) {
                    String val = split.value(++i);
                    if (flags & Escape)
                        val = unquote(val);
                    arguments.append(Path::resolved(val, Path::MakeAbsolute, path));
                }
            }
        } else {
            if (!seenCompiler) {
                seenCompiler = true;
            } else {
                Path input = Path::resolved(arg, Path::MakeAbsolute, path);
                if (input.isSource()) {
                    if (unresolvedInputLocations)
                        *unresolvedInputLocations << input;
                    input.resolve(Path::RealPath);
                    inputs.append(std::make_pair(Location::insertFile(input), input));
                }
            }
        }
    }

    if (inputs.isEmpty()) {
        warning() << "Source::parse No file for" << cmdLine;
        if (unresolvedInputLocations)
            unresolvedInputLocations->clear();
        return List<Source>();
    }

    if (!isIndexable(language)) {
        warning() << "Source::parse We can't index this" << language;
        return List<Source>();
    }

    // ### not threadsafe
    assert(EventLoop::isMainThread());
    static Hash<Path, Path> resolvedFromPath;
    Path front = split.front();
    Path &compiler = resolvedFromPath[front];
    if (compiler.isEmpty()) {
        // error() << "Coming in with" << front;
        if (front.startsWith('/')) {
            Path resolved = front.resolved();
            // error() << "got resolved to" << resolved;
            const char *fn = resolved.fileName();
            if (!strcmp(fn, "gcc-rtags-wrapper.sh") || !strcmp(fn, "icecc")) {
                front = front.fileName();
                // error() << "We're set at" << front;
            } else {
                compiler = resolved;
            }
        }
        if (!front.startsWith('/') && !front.isEmpty()) {
            static const char* path = getenv("PATH");
            if (path) {
                static const List<String> paths = String(path).split(':');
                for (List<String>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
                    bool ok;
                    const Path p = Path::resolved(front, Path::RealPath, *it, &ok);
                    if (ok) {
                        const char *fn = p.fileName();
                        if (strcmp(fn, "gcc-rtags-wrapper.sh") && strcmp(fn, "icecc")
                            && !access(p.nullTerminated(), R_OK | X_OK)) {
                            // error() << "Found it at" << compiler;
                            compiler = p;
                            break;
                        }
                    }
                }
            }
        }
        // error() << "Got compiler" << split.front() << "==>" << compiler;
        if (compiler.isEmpty()) {
            compiler = split.front();
        }
    }

    // error() << split.front() << front << compiler;
    compilerId = Location::insertFile(compiler);
    List<Source> ret;
    if (!inputs.isEmpty()) {
        if (!buildRootId) {
            buildRoot = RTags::findProjectRoot(inputs.first().second, RTags::BuildRoot);
            buildRoot.resolve(Path::RealPath);
            buildRootId = Location::insertFile(buildRoot);
        }
        includePathHash = ::hashIncludePaths(includePaths, buildRoot);

        ret.resize(inputs.size());
        int idx = 0;
        for (const auto input : inputs) {
            Source &source = ret[idx++];
            source.fileId = input.first;
            source.compilerId = compilerId;
            source.buildRootId = buildRootId;
            source.includePathHash = includePathHash;
            source.flags = sourceFlags;
            source.defines = defines;
            source.includePaths = includePaths;
            source.arguments = arguments;
            source.sysRootIndex = sysRootIndex;
            source.language = hasDashX ? language : guessLanguageFromSourceFile(input.second, language);
        }
    }
    return ret;
}

static inline bool compareDefinesNoNDEBUG(const Set<Source::Define> &l, const Set<Source::Define> &r)
{
    for (const auto &ld : l) {
        if (ld.define != "NDEBUG") {
            continue;
        } else if (!r.contains(ld)) {
            return false;
        }
    }
    return true;
}

static bool nextArg(List<String>::const_iterator &it,
                    const List<String>::const_iterator end,
                    bool separateDebugAndRelease)
{
    while (it != end) {
        if (isBlacklisted(*it)) {
            const bool val = hasValue(*it);
            ++it;
            if (val && it != end)
                ++it;
        } else if (!separateDebugAndRelease && (*it == "-g" || it->startsWith("-O"))) {
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
        return false;
    }

    const bool separateDebugAndRelease = Server::instance()->options().options & Server::SeparateDebugAndRelease;
    if (separateDebugAndRelease) {
        if (defines != other.defines)
            return false;
    } else if (!compareDefinesNoNDEBUG(defines, other.defines)) {
        return false;
    }

    auto me = arguments.begin();
    const auto myEnd = arguments.end();
    auto him = other.arguments.begin();
    const auto hisEnd = other.arguments.end();

    while (me != him) {
        if (!nextArg(me, myEnd, separateDebugAndRelease))
            break;
        if (!nextArg(him, hisEnd, separateDebugAndRelease))
            return false;
        if (*me != *him)
            return false;
        ++me;
        ++him;
    }
    return him == hisEnd || !nextArg(him, hisEnd, separateDebugAndRelease);
}

List<String> Source::toCommandLine(unsigned int flags) const
{
    const auto *options = Server::instance() ? &Server::instance()->options() : 0;
    if (!options)
        flags |= (ExcludeDefaultArguments|ExcludeDefaultDefines|ExcludeDefaultIncludePaths);

    List<String> ret;
    ret.reserve(64);
    if (flags & IncludeCompiler)
        ret.append(compiler());
    for (int i=0; i<arguments.size(); ++i) {
        if (!(flags & FilterBlacklist) || !isBlacklisted(arguments.at(i))) {
            ret.append(arguments.at(i));
        } else if (hasValue(arguments.at(i))) {
            ++i;
        }
    }
    if (!(flags & ExcludeDefaultArguments)) {
        for (const auto &arg : options->defaultArguments)
            ret.append(arg);
    }

    if (flags & IncludeDefines) {
        for (const auto &def : defines)
            ret += def.toString(flags);
        if (!(flags & ExcludeDefaultIncludePaths)) {
            for (const auto &def : options->defines)
                ret += def.toString(flags);
        }
    }
    if (flags & IncludeIncludepaths) {
        for (const auto &inc : includePaths) {
            switch (inc.type) {
            case Source::Include::Type_None:
                assert(0 && "Impossible impossibility");
                break;
            case Source::Include::Type_Include:
                ret << ("-I" + inc.path);
                break;
            case Source::Include::Type_Framework:
                ret << ("-F" + inc.path);
                break;
            case Source::Include::Type_System:
                ret << "-isystem" << inc.path;
                break;
            case Source::Include::Type_SystemFramework:
                ret << "-iframework" << inc.path;
                break;
            }
        }
        if (!(flags & ExcludeDefaultIncludePaths)) {
            for (const auto &inc : options->includePaths) {
                switch (inc.type) {
                case Source::Include::Type_None:
                    assert(0 && "Impossible impossibility");
                    break;
                case Source::Include::Type_Include:
                    ret << ("-I" + inc.path);
                    break;
                case Source::Include::Type_Framework:
                    ret << ("-F" + inc.path);
                    break;
                case Source::Include::Type_System:
                    ret << "-isystem" << inc.path;
                    break;
                case Source::Include::Type_SystemFramework:
                    ret << "-iframework" << inc.path;
                    break;
                }
            }
        }
    }
    if (flags & IncludeSourceFile)
        ret.append(sourceFile());

    return ret;
}
