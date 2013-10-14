/* This file is part of RTags.

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

#include "GccArguments.h"
#include <rct/Log.h>
#include "RTags.h"
#include <rct/Process.h>
#include "Server.h"
#include "SourceInformation.h"

GccArguments::GccArguments()
    : mLanguage(NoLanguage)
{
}

void GccArguments::clear()
{
    mClangArgs.clear();
    mInputFiles.clear();
    mUnresolvedInputFiles.clear();
    mBase.clear();
    mCompiler.clear();
    mLanguage = NoLanguage;
}

GccArguments::Language GccArguments::guessLanguageFromCompiler(const Path &fullPath)
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

    GccArguments::Language lang = GccArguments::NoLanguage;
    if (c.startsWith("g++") || c.startsWith("c++") || c.startsWith("clang++")) {
        lang = GccArguments::CPlusPlus;
    } else if (c.startsWith("gcc") || c.startsWith("cc") || c.startsWith("clang")) {
        lang = GccArguments::C;
    }
    return lang;
}

GccArguments::Language GccArguments::guessLanguageFromSourceFile(const Path &sourceFile)
{
    const char *suffix = sourceFile.extension();
    if (suffix) {
        if (!strcasecmp(suffix, "cpp")) {
            return CPlusPlus;
        } else if (!strcasecmp(suffix, "cc")) {
            return CPlusPlus;
        } else if (!strcmp(suffix, "C")) {
            return CPlusPlus;
        } else if (!strcmp(suffix, "c")) {
            return C;
        }
    }
    return NoLanguage;
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

bool GccArguments::parse(String args, const Path &base)
{
    char quote = '\0';
    List<String> split;
    String old2 = args;
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
    debug() << "GccArguments::parse (" << args << ") => " << split;
    return parse(split, base);
}

bool GccArguments::parse(List<String> split, const Path &base)
{
    mLanguage = NoLanguage;
    mClangArgs.clear();
    mInputFiles.clear();
    mBase = base;

    eatAutoTools(split);

    if (split.isEmpty()) {
        clear();
        return false;
    }

    Path path;
    if (split.front() == "cd" && split.size() > 3 && split.at(2) == "&&") {
        path = Path::resolved(split.at(1), Path::MakeAbsolute, base);
        split.erase(split.begin(), split.begin() + 3);
    } else {
        path = base;
    }
    if (split.isEmpty()) {
        return false;
    }

    if (split.first().endsWith("rtags-gcc-prefix.sh")) {
        if (split.size() == 1) {
            return false;
        }
        split.removeAt(0);
    }

    mLanguage = guessLanguageFromCompiler(split.front());

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
                    mLanguage = CHeader;
                } else if (a == "c++-header") {
                    mLanguage = CPlusPlusHeader;
                } else if (a == "c") {
                    mLanguage = C;
                } else if (a == "c++") {
                    mLanguage = CPlusPlus;
                } else {
                    return false;
                }
                mClangArgs.append("-x");
                mClangArgs.append(a);
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
                mDefines.append(define);
                mClangArgs.append(a);
            } else if (arg.startsWith("-I")) {
                Path inc;
                bool ok = false;
                if (arg.size() > 2) {
                    inc = Path::resolved(arg.mid(2), Path::RealPath, path, &ok);
                } else {
                    inc = Path::resolved(split.value(++i), Path::RealPath, path, &ok);
                }
                mIncludePaths.append(inc);
                if (ok)
                    mClangArgs.append("-I" + inc);
            } else if (arg == "-m32") {
                mClangArgs.append(arg);
            } else if (arg.startsWith("-std=")) {
                mClangArgs.append(arg);
                if (arg == "-std=c++0x" || arg == "-std=c++11" || arg == "-std=gnu++0x" || arg == "-std=gnu++11") {
                    if (mLanguage == CPlusPlusHeader) {
                        mLanguage = CPlusPlus11Header;
                    } else {
                        mLanguage = CPlusPlus11;
                    }
                }
            } else if (arg.startsWith("-include")) {
                mClangArgs.append(arg);
                if (arg.size() == 8) {
                    mClangArgs.append(split.value(++i));
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
                mClangArgs.append(arg.left(from));
                mClangArgs.append(inc);
            } else if (arg.startsWith("-W")) {
                const bool hasComma = arg.contains(',');
                if (!hasComma) { // We don't want options such as -Wl,foo
                    mClangArgs.append(arg);
                }
            } else if (arg == "-w") {
                mClangArgs.append(arg);
            }
        } else {
            if (!seenCompiler) {
                seenCompiler = true;
            } else {
                Path input = Path::resolved(arg, Path::MakeAbsolute, path);
                if (input.isSource()) {
                    if (mLanguage == NoLanguage)
                        mLanguage = guessLanguageFromSourceFile(input);
                    mUnresolvedInputFiles.append(input);
                    input.resolve(Path::RealPath);
                    mInputFiles.append(input);
                }
            }
        }
    }

    if (mUnresolvedInputFiles.isEmpty()) {
        clear();
        return false;
    }

    if (mInputFiles.isEmpty()) {
        error("Unable to find or resolve input files");
        const int c = mUnresolvedInputFiles.size();
        for (int i=0; i<c; ++i) {
            const String &input = mUnresolvedInputFiles.at(i);
            error("  %s", input.constData());
        }
        clear();
        return false;
    }

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
    mCompiler = compiler;
    return true;
}

void GccArguments::init(const SourceInformation &sourceInformation)
{
    clear();
}

void GccArguments::addFlags(const List<String> &extraFlags)
{
    const int count = extraFlags.size();
    for (int i=0; i<count; ++i) {
        String flag = extraFlags.at(i);
        if (flag.startsWith("-I")) {
            Path p = Path::resolved(flag.constData() + 2);
            flag.replace(2, flag.size() - 2, p);
        }
        mClangArgs.append(flag);
    }
}

Path GccArguments::projectRoot() const
{
    const List<Path> *files[] = { &mUnresolvedInputFiles, &mInputFiles };
    for (int i=0; i<2; ++i) {
        const List<Path> &list = *files[i];
        for (int j=0; j<list.size(); ++j) {
            Path src = list.at(j);
            if (!src.isAbsolute())
                src.prepend(mBase);
            Path srcRoot = RTags::findProjectRoot(src);
            if (!srcRoot.isEmpty()) {
                return srcRoot;
            }
        }
    }
    return Path();
}

template <> inline Serializer &operator<<(Serializer &s, const GccArguments::Define &d)
{
    s << d.define << d.value;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, GccArguments::Define &d)
{
    s >> d.define >> d.value;
    return s;
}

void GccArguments::encode(Serializer &serializer) const
{
    serializer << mClangArgs << mDefines << mInputFiles << mUnresolvedInputFiles
               << mIncludePaths << mBase << mCompiler << static_cast<uint8_t>(mLanguage);
}

void GccArguments::decode(Deserializer &deserializer)
{
    uint8_t language;
    deserializer >> mClangArgs >> mDefines >> mInputFiles >> mUnresolvedInputFiles
                 >> mIncludePaths >> mBase >> mCompiler >> language;
    mLanguage = static_cast<Language>(language);
}
