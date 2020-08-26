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
#include "rct/Set.h"
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

    Set<Source::Define> defines;
    List<Source::Include> includePaths;
    List<Source::Include> stdincxxPaths;
    List<Source::Include> builtinPaths;
};
static Hash<Path, Compiler> sCompilers;

namespace CompilerManager {

List<Path> compilers()
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

        List<String> overrides;
        List<String> out, err;
        List<String> args;
        List<String> environ({"RTAGS_DISABLED=1"});
        args << "-x" << "c++" << "-v" << "-E" << "-dM" << "-";

        for (int i=0; i<4; /* see below */) {
            Process proc;
            proc.exec(cpath, args, environ);
            assert(proc.isFinished());
            if (!proc.returnCode()) {
                out << proc.readAllStdOut().split('\n');
                err << proc.readAllStdErr().split('\n');

                // proc success. What's next?
                switch (i) {
                case 0:
                    // C++ ok .. see which path is controlled by -nostdinc++
                    args.prepend("-nostdinc++");
                    err << "@@@@\n"; // magic separator
                    i = 2;
                    break;

                case 1:
                    // "-x c++" not ok. Goto -nobuiltininc.
                    err << "@@@@\n";  // magic separator
                    args.prepend("-nobuiltininc");
                    i = 3;
                    break;

                case 2:
                    args.removeFirst(); // clear -nostdinc++
                    err << "@@@@\n";  // magic separator
                    args.prepend("-nobuiltininc");
                    i = 3;
                    break;

                default:
                    err << "@@@@\n";  // magic separator
                    i = 4;
                    break;
                }
            } else if (i == 0) {
                // Strip -x c++ and try again
                args.removeFirst();
                args.removeFirst();
                i = 1;
            } else if (i == 3) {
                // GCC does not support -nobuiltininc flag.
                // Remove and retry
                args.removeFirst();
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
                const int space = line.indexOf(' ', 8);
                if (space == -1) {
                    def.define = line.mid(8);
                } else {
                    def.define = line.mid(8, space - 8);
                    def.value = line.mid(space + 1);
                }
                compiler.defines.insert(def);
            }
        }

        enum { eNormal, eNoStdInc, eNoBuiltin } mode = eNormal;
        List<Source::Include> copy;
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
                    for (auto& inc : compiler.stdincxxPaths)
                        compiler.includePaths.remove(inc);
                    for (auto& inc : compiler.builtinPaths)
                        compiler.includePaths.remove(inc);
                    break; // we're done
                } else {
                    mode = eNoStdInc;
                }
                copy = compiler.includePaths;
            }
            size_t j = 0;
            while (j < line.size() && isspace(line.at(j)))
                ++j;
            int end = line.lastIndexOf(" (framework directory)");
            Source::Include::Type type = Source::Include::Type::Type_System;
            if (end != -1) {
                end = end - j;
                type = Source::Include::Type_SystemFramework;
            }
            Path path = line.mid(j, end);
            // error() << "looking at" << line << path << path.isDir();
            if (path.isDir()) {
                path.resolve();
                if (mode == eNormal) {
                    compiler.includePaths.append(Source::Include(type, path));
                } else {
                    copy.remove(Source::Include(type, path));
                }
            }
        }
        debug() << "[CompilerManager]" << cpath << "got includepaths\n" << compiler.includePaths;
        debug() << "StdInc++: " << compiler.stdincxxPaths << "\nBuiltin: " << compiler.builtinPaths;
        debug() << "[CompilerManager] returning.\n";
    }
    if (flags & IncludeDefines)
        source.defines << compiler.defines;
    if (flags & IncludeIncludePaths) {
        if (!source.arguments.contains("-nostdinc")) {
            source.includePaths << compiler.includePaths;
            if (!source.arguments.contains("-nostdinc++"))
                source.includePaths << compiler.stdincxxPaths;
            if (!source.arguments.contains("-nobuiltininc"))
                source.includePaths << compiler.builtinPaths;
        } else if (!strncmp("clang", cpath.fileName(), 5)) {
            // Module.map causes errors when -nostdinc is used, as it
            // can't find some mappings to compiler provided headers
            source.arguments.append("-fno-modules");
        }
    }
}

} // namespace CompilerManager
