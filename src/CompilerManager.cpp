#include "CompilerManager.h"
#include "Process.h"
#include "MutexLocker.h"

Mutex sMutex;
Map<Path, List<ByteArray> > sFlags;

namespace CompilerManager {
List<ByteArray> flags(const Path &compiler, const Path &cppOverride)
{
    MutexLocker lock(&sMutex);
    Map<Path, List<ByteArray> >::const_iterator it = sFlags.find(compiler);
    if (it != sFlags.end())
        return it->second;

    Path cpp = cppOverride;
    if (cpp.isEmpty()) {
        cpp = compiler.parentDir() + "cpp";
        if (!(cpp.mode() & 0x111)) { // not pretty
            cpp = "cpp";
        }
    }

    it = sFlags.find(cpp);
    if (it != sFlags.end())
        return it->second;

    List<ByteArray> out;
    for (int i=0; i<2; ++i) {
        Process proc;
        List<ByteArray> args;
        if (i == 0)
            args << "-x" << "c++";
        args << "-v";
        proc.start(cpp, args);
        proc.closeStdIn();
        while (!proc.isFinished())
            usleep(100000); // ### this is not particularly nice
        assert(proc.isFinished());
        if (!proc.returnCode()) {
            out = proc.readAllStdErr().split('\n');
            break;
        } else if (i == 1) {
            out = proc.readAllStdErr().split('\n');
        }
    }
    List<ByteArray> &flags = sFlags[compiler];
    for (int i=0; i<out.size(); ++i) {
        const ByteArray &line = out.at(i);
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
    sFlags[cpp] = flags;
    warning() << compiler << cpp << "got\n" << ByteArray::join(flags, "\n");

    return flags;
}
}
