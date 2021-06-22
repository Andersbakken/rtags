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

#include "CompilerManager.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <mutex>

#include "rct/Log.h"
#include "rct/Process.h"
#include "Source.h"
#include "rct/Hash.h"
#include "rct/Path.h"
#include <set>
#include "rct/String.h"

static std::mutex sMutex;
struct Compiler {
    Compiler()
        : inited(false)
    {}
    bool inited;

    // There are three include-path-limiting options:
    //   1. -nostdinc      -- disables all default system include paths
    //                        Example: <string.h> (stuff under /usr/include, etc.)
    //   2. -nostdinc++    -- disables C++ standard include path
    //                        Example: <vector> (stuff under /usr/lib/c++/v1)
    //   3. -nobuiltininc  -- (clang only?) disables compiler-provided includes
    //                        Example: limits.h, float.h

    std::set<Source::Define> defines;
    std::vector<Source::Include> includePaths;
    std::vector<Source::Include> stdincxxPaths;
    std::vector<Source::Include> builtinPaths;
};
static Hash<Path, Compiler> sCompilers;

namespace CompilerManager {

std::vector<Path> compilers()
{
    std::lock_guard<std::mutex> lock(sMutex);
    return sCompilers.keys();
}

void applyToSource(Source &source, Flags<CompilerManager::Flag> flags)
{
    std::lock_guard<std::mutex> lock(sMutex);
    Path cpath = source.compiler();
    Compiler &compiler = sCompilers[cpath];
    if (!compiler.inited) {
        compiler.inited = true;

        std::vector<String> overrides;
        std::vector<String> out, err;
        std::vector<String> args;
        std::vector<String> environ({"RTAGS_DISABLED=1"});
        args.push_back("-x");
        args.push_back("c++");
        args.push_back("-v");
        args.push_back("-E");
        args.push_back("-dM");
        args.push_back("-");

        for (size_t i=0; i<4; /* see below */) {
            Process proc;
            proc.exec(cpath, args, environ);
            assert(proc.isFinished());
            if (!proc.returnCode()) {
                std::vector<String> split = proc.readAllStdOut().split('\n');
                out.insert(out.end(), split.begin(), split.end());
                split = proc.readAllStdErr().split('\n');
                err.insert(err.end(), split.begin(), split.end());

                // proc success. What's next?
                switch (i) {
                case 0:
                    // C++ ok .. see which path is controlled by -nostdinc++
                    args.insert(args.begin(), "-nostdinc++");
                    err.push_back("@@@@\n"); // magic separator
                    i = 2;
                    break;

                case 1:
                    // "-x c++" not ok. Goto -nobuiltininc.
                    err.push_back("@@@@\n");  // magic separator
                    args.insert(args.begin(), "-nobuiltininc");
                    i = 3;
                    break;

                case 2:
                    args.erase(args.begin()); // clear -nostdinc++
                    err.push_back("@@@@\n");  // magic separator
                    args.insert(args.begin(), "-nobuiltininc");
                    i = 3;
                    break;

                default:
                    err.push_back("@@@@\n");  // magic separator
                    i = 4;
                    break;
                }
            } else if (i == 0) {
                // Strip -x c++ and try again
                args.erase(args.begin(), args.begin() + 1);
                i = 1;
            } else if (i == 3) {
                // GCC does not support -nobuiltininc flag.
                // Remove and retry
                args.erase(args.begin());
            } else {
                error() << "CompilerManager: Cannot extract standard include paths.\n";
                return;
            }
        }
        for (size_t i=0; i<out.size(); ++i) {
            const String &line = out.at(i);
            // error() << c << line;
            if (line.startsWith("#define ")) {
                Source::Define def;
                const size_t space = line.indexOf(' ', 8);
                if (space == std::numeric_limits<size_t>::max()) {
                    def.define = line.mid(8);
                } else {
                    def.define = line.mid(8, space - 8);
                    def.value = line.mid(space + 1);
                }
                compiler.defines.insert(def);
            }
        }

        enum { eNormal, eNoStdInc, eNoBuiltin } mode = eNormal;
        std::vector<Source::Include> copy;
        for (size_t i=0; i<err.size(); ++i) {
            const String &line = err.at(i);
            if (line.startsWith("@@@@")) { // magic separator
                if (mode == eNoStdInc) {
                    // What's left in copy are the std c++ paths
                    compiler.stdincxxPaths = copy;
                    mode = eNoBuiltin;
                } else if (mode == eNoBuiltin) {
                    // What's left in copy are the builtin paths
                    compiler.builtinPaths = copy;
                    // Set the includePaths exclusive of stdinc/builtin
                    for (const auto& inc : compiler.stdincxxPaths) {
                        auto it = std::find(compiler.includePaths.begin(), compiler.includePaths.end(), inc);
                        if (it != compiler.includePaths.end())
                            compiler.includePaths.erase(it);
                    }
                    for (const auto& inc : compiler.builtinPaths) {
                        auto it = std::find(compiler.includePaths.begin(), compiler.includePaths.end(), inc);
                        if (it != compiler.includePaths.end())
                            compiler.includePaths.erase(it);
                    }
                    break; // we're done
                } else {
                    mode = eNoStdInc;
                }
                copy = compiler.includePaths;
            }
            size_t j = 0;
            while (j < line.size() && isspace(line.at(j)))
                ++j;
            size_t end = line.lastIndexOf(" (framework directory)");
            Source::Include::Type type = Source::Include::Type::Type_System;
            if (end != std::numeric_limits<size_t>::max()) {
                end = end - j;
                type = Source::Include::Type_SystemFramework;
            }
            Path path = line.mid(j, end);
            // error() << "looking at" << line << path << path.isDir();
            if (path.isDir()) {
                path.resolve();
                if (mode == eNormal) {
                    compiler.includePaths.push_back(Source::Include(type, path));
                } else {
                    auto it = std::find(copy.begin(), copy.end(), Source::Include(type, path));
                    if (it != copy.end())
                        copy.erase(it);
                }
            }
        }
        debug() << "[CompilerManager]" << cpath << "got includepaths\n" << compiler.includePaths;
        debug() << "StdInc++: " << compiler.stdincxxPaths << "\nBuiltin: " << compiler.builtinPaths;
        debug() << "[CompilerManager] returning.\n";
    }
    if (flags & IncludeDefines) {
        for (const auto &ref : compiler.defines) {
            source.defines.insert(ref);
        }
    }
    if (flags & IncludeIncludePaths) {
        if (std::find(source.arguments.begin(), source.arguments.end(), "-nostdinc") == source.arguments.end()) {
            if (std::find(source.arguments.begin(), source.arguments.end(), "-nostdinc++") == source.arguments.end()) {
                source.includePaths.insert(source.includePaths.end(), compiler.stdincxxPaths.begin(), compiler.stdincxxPaths.end());
            }
            if (std::find(source.arguments.begin(), source.arguments.end(), "-nobuiltininc") == source.arguments.end()) {
                source.includePaths.insert(source.includePaths.end(), compiler.builtinPaths.begin(), compiler.builtinPaths.end());
            }
            source.includePaths.insert(source.includePaths.end(), compiler.includePaths.begin(), compiler.includePaths.end());
        } else if (!strncmp("clang", cpath.fileName(), 5)) {
            // Module.map causes errors when -nostdinc is used, as it
            // can't find some mappings to compiler provided headers
            source.arguments.push_back("-fno-modules");
        }
    }
}

} // namespace CompilerManager
