#include "Source.h"
#include "Location.h"

void Source::clear()
{
    fileId = compilerId = 0;
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

Path Source::compiler() const
{
    return Location::path(compilerId);
}

bool Source::compare(const Source &other) const // ignores parsed
{
    return (fileId == other.fileId
            && compilerId == other.compilerId
            && language == other.language
            && defines == other.defines
            && includePaths == other.includePaths
            && arguments == other.arguments);
}

List<String> Source::toCommandLine(unsigned int mode) const
{
    int count = arguments.size() + defines.size() + includePaths.size();
    if (mode & IncludeCompiler)
        ++count;
    if (mode & IncludeSourceFile)
        ++count;
    List<String> ret;
    ret.reserve(count);
    if (mode & IncludeCompiler)
        ret.append(compiler());
    ret += arguments;
    for (List<Define>::const_iterator it = defines.begin(); it != defines.end(); ++it)
        ret += it->toString();
    for (List<Path>::const_iterator it = includePaths.begin(); it != includePaths.end(); ++it)
        ret += ("-I" + *it);

    return ret;
}

String Source::toString() const
{
    String ret = String::join(toCommandLine(IncludeCompiler|IncludeSourceFile), ' ');
    if (parsed) {
        ret += " Parsed: " + String::formatTime(parsed, String::DateTime);
    }
    return ret;
}

static inline Source::Language guessLanguageFromCompiler(const Path &fullPath)
{
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

static inline Source::Language guessLanguageFromSourceFile(const Path &sourceFile)
{
    const char *suffix = sourceFile.extension();
    if (suffix) {
        if (!strcasecmp(suffix, "cpp")) {
            return Source::CPlusPlus;
        } else if (!strcasecmp(suffix, "cc")) {
            return Source::CPlusPlus;
        } else if (!strcmp(suffix, "C")) {
            return Source::CPlusPlus;
        } else if (!strcmp(suffix, "c")) {
            return Source::C;
        }
    }
    return Source::NoLanguage;
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

Source Source::parse(const String &cmdLine, const Path &base, Path *unresolvedInputLocation)
{
    String args = cmdLine;
    char quote = '\0';
    List<String> split;
    {
        char *cur = args.data();
        char *prev = cur;
        // ### handle escaped quotes?
        int size = args.size();
        while (size > 0) {
            switch (*cur) {
            case '"':
            case '\'':
                if (quote == '\0')
                    quote = *cur;
                else if (*cur == quote)
                    quote = '\0';
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
    debug() << "Source::parse (" << args << ") => " << split;

    eatAutoTools(split);

    if (split.isEmpty()) {
        return Source();
    }

    Path path;
    if (split.front() == "cd" && split.size() > 3 && split.at(2) == "&&") {
        path = Path::resolved(split.at(1), Path::MakeAbsolute, base);
        split.erase(split.begin(), split.begin() + 3);
    } else {
        path = base;
    }
    if (split.isEmpty()) {
        return Source();
    }

    if (split.first().endsWith("rtags-gcc-prefix.sh")) {
        if (split.size() == 1) {
            return Source();
        }
        split.removeAt(0);
    }

    Source ret;
    ret.language = guessLanguageFromCompiler(split.front());

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
            if (arg == "-x") {
                const String a = split.value(++i);
                if (a == "c-header") {
                    ret.language = CHeader;
                } else if (a == "c++-header") {
                    ret.language = CPlusPlusHeader;
                } else if (a == "c") {
                    ret.language = C;
                } else if (a == "c++") {
                    ret.language = CPlusPlus;
                } else {
                    return Source();
                }
                ret.arguments.append("-x");
                ret.arguments.append(a);
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
                const int eq = def.indexOf('=');
                if (eq == -1) {
                    define.define = def;
                } else {
                    define.define = def.left(eq);
                    define.value = def.mid(eq + 1);
                }
                ret.defines.append(define);
                // ret.arguments.append(a);
            } else if (arg.startsWith("-I")) {
                Path inc;
                bool ok = false;
                if (arg.size() > 2) {
                    inc = Path::resolved(arg.mid(2), Path::RealPath, path, &ok);
                } else {
                    inc = Path::resolved(split.value(++i), Path::RealPath, path, &ok);
                }
                ret.includePaths.append(inc);
                // if (ok)
                //     ret.arguments.append("-I" + inc);
            } else if (arg == "-m32") {
                ret.arguments.append(arg);
            } else if (arg.startsWith("-std=")) {
                ret.arguments.append(arg);
                // error() << "Got std" << arg;
                if (arg == "-std=c++0x" || arg == "-std=c++11" || arg == "-std=gnu++0x" || arg == "-std=gnu++11") {
                    if (ret.language == CPlusPlusHeader) {
                        ret.language = CPlusPlus11Header;
                    } else {
                        ret.language = CPlusPlus11;
                    }
                }
            } else if (arg.startsWith("-include")) {
                ret.arguments.append(arg);
                if (arg.size() == 8) {
                    ret.arguments.append(split.value(++i));
                }
            } else if (arg.startsWith("-isystem") || arg.startsWith("-iquote")) {
                const int from = (arg[2] == 'q' ? 7 : 8);
                assert(arg.size() >= from);
                Path inc;
                if (arg.size() > from) {
                    bool ok = false;
                    inc = Path::resolved(arg.mid(from), Path::RealPath, path, &ok);
                    if (!ok)
                        inc = arg.mid(from);
                } else if (i + 1 < s) {
                    bool ok = false;
                    inc = Path::resolved(split.value(++i), Path::RealPath, path, &ok);
                    if (!ok)
                        inc = split.at(i);
                }
                // ### need to add to includepaths
                ret.arguments.append(arg.left(from));
                ret.arguments.append(inc);
            } else if (arg.startsWith("-W")) {
                const bool hasComma = arg.contains(',');
                if (!hasComma) { // We don't want options such as -Wl,foo
                    ret.arguments.append(arg);
                }
            } else if (arg == "-w") {
                ret.arguments.append(arg);
            }
        } else {
            if (!seenCompiler) {
                seenCompiler = true;
            } else if (ret.fileId) {
                return Source();
            } else {
                Path input = Path::resolved(arg, Path::MakeAbsolute, path);
                if (input.isSource()) {
                    if (ret.language == NoLanguage)
                        ret.language = guessLanguageFromSourceFile(input);
                    if (unresolvedInputLocation)
                        *unresolvedInputLocation = input;
                    input.resolve(Path::RealPath);
                    ret.fileId = Location::insertFile(input);
                }
            }
        }
    }

    if (!ret.fileId)
        return Source();

    // ### not threadsafe
    static Hash<Path, Path> resolvedFromPath;
    const String &front = split.front();
    Path &compiler = resolvedFromPath[front];
    if (compiler.isEmpty()) {
        if (!front.startsWith('/') && !front.isEmpty()) {
            static const char* path = getenv("PATH");
            if (path) {
                static const List<String> paths = String(path).split(':');
                for (List<String>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
                    bool ok;
                    const Path ret = Path::resolved(front, Path::RealPath, *it, &ok);
                    if (ok) {
                        const char *fn = ret.fileName();
                        if (strcmp(fn, "gcc-rtags-wrapper.sh") && strcmp(fn, "icecc")
                            && !access(ret.nullTerminated(), R_OK | X_OK)) {
                            compiler = ret;
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
    ret.compilerId = Location::insertFile(compiler);
    return ret;
}
