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

#include "CompilerManager.h"
#include <rct/Process.h>
#include "Server.h"

static std::mutex sMutex;
static Hash<Path, List<String> > sFlags;

namespace CompilerManager {
List<Path> compilers()
{
    std::lock_guard<std::mutex> lock(sMutex);
    return sFlags.keys();
}

List<String> flags(const Path &compiler)
{
    std::lock_guard<std::mutex> lock(sMutex);
    enum {
        Unset,
        Use,
        DontUse
    } sUseCompilerFlags = Unset;

    if (sUseCompilerFlags == Unset)
        sUseCompilerFlags = Server::instance()->options().options & Server::UseCompilerFlags ? Use : DontUse;

    if (sUseCompilerFlags == DontUse)
        return List<String>();

    Hash<Path, List<String> >::const_iterator it = sFlags.find(compiler);
    if (it != sFlags.end())
        return it->second;

    List<String> out;
    for (int i=0; i<2; ++i) {
        Process proc;
        List<String> args;
        List<String> environ;
        environ << "PATH=/usr/local/bin:/usr/bin";
        if (i == 0) {
            args << "-v" << "-x" << "c++" << "-E" << "-";
        } else {
            if (i == 0)
                args << "-v" << "-E" << "-";
        }
        proc.exec(compiler, args, environ);
        assert(proc.isFinished());
        if (!proc.returnCode()) {
            out = proc.readAllStdErr().split('\n');
            break;
        } else if (i == 1) {
            out = proc.readAllStdErr().split('\n');
        }
    }
    List<String> &flags = sFlags[compiler];
    for (int i=0; i<out.size(); ++i) {
        const String &line = out.at(i);
        int j = 0;
        while (j < line.size() && isspace(line.at(j)))
            ++j;
        Path path = line.mid(j);
        if (path.isDir()
#ifdef OS_Darwin
            && !path.contains("/lib/clang/")
#endif
            )
        {
            path.resolve();
            path.prepend("-I");
            flags.append(path);
        }
    }
    warning() << compiler << "got\n" << String::join(flags, "\n");

    return flags;
}
}
