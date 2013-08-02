#include "CompilerManager.h"
#include <rct/Process.h>
#include <rct/MutexLocker.h>
#include "Server.h"

static Mutex sMutex;
static Map<Path, List<String> > sFlags;

namespace CompilerManager {
List<Path> compilers()
{
    MutexLocker lock(&sMutex);
    return sFlags.keys();
}

List<String> flags(const Path &compiler)
{
    MutexLocker lock(&sMutex);
    enum {
        Unset,
        Use,
        DontUse
    } sUseCompilerFlags = Unset;

    if (sUseCompilerFlags == Unset)
        sUseCompilerFlags = Server::instance()->options().options & Server::UseCompilerFlags ? Use : DontUse;

    if (sUseCompilerFlags == DontUse)
        return List<String>();

    Map<Path, List<String> >::const_iterator it = sFlags.find(compiler);
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
        if (path.isDir()) {
            path.resolve();
            path.prepend("-I");
            flags.append(path);
        }
    }
    warning() << compiler << "got\n" << String::join(flags, "\n");

    return flags;
}
}
