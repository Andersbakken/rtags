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

GccArguments::GccArguments()
    : mLang(NoLang)
{
}

void GccArguments::clear()
{
    mClangArgs.clear();
    mInputFiles.clear();
    mUnresolvedInputFiles.clear();
    mBase.clear();
    mCompiler.clear();
    mLang = NoLang;
}

static inline GccArguments::Lang guessLang(const Path &fullPath)
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

    GccArguments::Lang lang = GccArguments::NoLang;
    if (c.startsWith("g++") || c.startsWith("c++") || c.startsWith("clang++")) {
        lang = GccArguments::CPlusPlus;
    } else if (c.startsWith("gcc") || c.startsWith("cc") || c.startsWith("clang")) {
        lang = GccArguments::C;
    }
    return lang;
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

static inline void addIncludeArg(List<String> &clangArgs, int argLen, const List<String> &args, int &idx, const Path &cwd)
{
    const String &arg = args.at(idx);
    if (arg.size() == argLen) {
        clangArgs.append(arg);
        clangArgs.append(Path::resolved(args.at(++idx), Path::MakeAbsolute, cwd));
    } else {
        clangArgs.append(arg.left(argLen) + Path::resolved(arg.mid(argLen), Path::MakeAbsolute, cwd));
    }
}

bool GccArguments::parse(String args, const Path &base)
{
    mLang = NoLang;
    mClangArgs.clear();
    mInputFiles.clear();
    mBase = base;

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
    eatAutoTools(split);

    if (split.isEmpty()) {
        clear();
        return false;
    }
    debug() << "GccArguments::parse (" << args << ") => " << split;

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

    mLang = guessLang(split.front());
    if (mLang == NoLang) {
        clear();
        return false;
    }

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
        if (arg.startsWith('-')) {
            if (arg.startsWith("-x")) {
                String a;
                if (arg.size() == 2 && i + 1 < s) {
                    a = split.at(++i);
                } else {
                    a = arg.mid(2);
                }
                if (a == "c-header" || a == "c++-header") {
                    return false;
                }
                mClangArgs.append("-x");
                mClangArgs.append(a);
            } else if (arg.startsWith("-I")) {
                addIncludeArg(mClangArgs, 2, split, i, path);
            } else if (arg.startsWith("-include") || arg.startsWith("-isystem")) {
                addIncludeArg(mClangArgs, 8, split, i, path);
            } else if (arg.startsWith("-iquote")) {
                addIncludeArg(mClangArgs, 7, split, i, path);
            } else if (arg.startsWith("-cxx-isystem")) {
                addIncludeArg(mClangArgs, 12, split, i, path);
            } else {
                mClangArgs.append(arg);
                if (arg == "-target" || arg == "-o")
                    mClangArgs.append(split.value(++i));
            }
        } else {
            if (!seenCompiler) {
                seenCompiler = true;
            } else {
                Path input = Path::resolved(arg, Path::MakeAbsolute, path);
                if (input.isSource()) {
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
    Path &compiler = resolvedFromPath[split.front()];
    if (compiler.isEmpty()) {
        compiler = Process::findCommand(split.front());
        if (compiler.isEmpty()) {
            compiler = split.front();
        }
    }
    mCompiler = compiler;
    return true;
}

GccArguments::Lang GccArguments::lang() const
{
    return mLang;
}

List<String> GccArguments::clangArgs() const
{
    return mClangArgs;
}

List<Path> GccArguments::inputFiles() const
{
    return mInputFiles;
}

List<Path> GccArguments::unresolvedInputFiles() const
{
    return mUnresolvedInputFiles;
}

Path GccArguments::baseDirectory() const
{
    return mBase;
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

Path GccArguments::compiler() const
{
    return mCompiler;
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
