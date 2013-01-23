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

    Process proc;
    // shared_ptr<Finished> f(new Finished);
    // proc.finished().connect(f.get(), &Finished::onFinished);
#ifdef OS_Darwin
    static const List<ByteArray> args = List<ByteArray>() << "-v";
#else
    static const List<ByteArray> args = List<ByteArray>() << "-x" << "c++" << "-v";
#endif
    proc.start(cpp, args);
    proc.closeStdIn();
    while (!proc.isFinished())
        usleep(100000); // ### this is not particularly nice
    assert(proc.isFinished());
    const List<ByteArray> out = proc.readAllStdErr().split('\n');
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
