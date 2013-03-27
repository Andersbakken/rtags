#include "CompilerManager.h"
#include <rct/Process.h>
#include <rct/MutexLocker.h>

static Mutex sMutex;
static Map<Path, List<String> > sFlags;
static Map<Path, Set<Path> > sAliases;
static bool sUseCompilerFlags = false;

namespace CompilerManager {
void init(const Server::Options &options)
{
    MutexLocker lock(&sMutex);
    sUseCompilerFlags = (options.options & Server::UseCompilerFlags);
}

List<Path> compilers()
{
    MutexLocker lock(&sMutex);
    return sFlags.keys();
}

List<String> flags(const Path &compiler)
{
    MutexLocker lock(&sMutex);

    if (!sUseCompilerFlags)
        return List<String>();

    Map<Path, List<String> >::const_iterator it = sFlags.find(compiler);
    if (it == sFlags.end()) {
        const Set<Path> aliases = sAliases.value(compiler);
        for (Set<Path>::const_iterator ait = aliases.begin(); ait != aliases.end(); ++ait) {
            it = sFlags.find(*ait);
            if (it != sFlags.end())
                break;
        }
    }

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
        proc.start(compiler, args, environ);
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

void addAliases(const Set<Path> &paths)
{
    MutexLocker lock(&sMutex);
    if (paths.size() > 1) {
        for (Set<Path>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
            Set<Path> copy = paths;
            copy.remove(*it);
            sAliases[*it] = copy;
        }
    }
}

Set<Path> aliases(const Path &path)
{
    MutexLocker lock(&sMutex);
    return sAliases.value(path);
}
}
