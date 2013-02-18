#include "StatusJob.h"
#include "CursorInfo.h"
#include "RTags.h"
#include "Server.h"
#include <clang-c/Index.h>
#include "Project.h"

const char *StatusJob::delimiter = "*********************************";
StatusJob::StatusJob(const QueryMessage &q, const shared_ptr<Project> &project)
    : Job(q, WriteUnfiltered|QuietJob, project), query(q.query())
{
}

void StatusJob::execute()
{
    bool matched = false;
    const char *alternatives = "fileids|dependencies|fileinfos|symbols|symbolnames|watchedpaths";
    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "fileids")) {
        matched = true;
        write(delimiter);
        write("fileids");
        write(delimiter);
        const Map<uint32_t, Path> paths = Location::idsToPaths();
        for (Map<uint32_t, Path>::const_iterator it = paths.begin(); it != paths.end(); ++it)
            write<256>("  %u: %s", it->first, it->second.constData());
        if (isAborted())
            return;
    }

    shared_ptr<Project> proj = project();
    if (!proj) {
        if (!matched) {
            write(alternatives);
        }
        return;
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "watchedpaths")) {
        matched = true;
        write(delimiter);
        write("watchedpaths");
        write(delimiter);
        const Set<Path> watched = proj->watchedPaths();
        for (Set<Path>::const_iterator it = watched.begin(); it != watched.end(); ++it) {
            write<256>("  %s", it->constData());
        }
        if (isAborted())
            return;
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "dependencies")) {
        matched = true;
        const DependencyMap map = proj->dependencies();
        write(delimiter);
        write("dependencies");
        write(delimiter);
        DependencyMap depsReversed;

        for (DependencyMap::const_iterator it = map.begin(); it != map.end(); ++it) {
            write<256>("  %s (%d) is depended on by", Location::path(it->first).constData(), it->first);
            const Set<uint32_t> &deps = it->second;
            for (Set<uint32_t>::const_iterator dit = deps.begin(); dit != deps.end(); ++dit) {
                write<256>("    %s (%d)", Location::path(*dit).constData(), *dit);
                depsReversed[*dit].insert(it->first);
            }
            if (isAborted())
                return;
        }
        for (DependencyMap::const_iterator it = depsReversed.begin(); it != depsReversed.end(); ++it) {
            write<256>("  %s (%d) depends on", Location::path(it->first).constData(), it->first);
            const Set<uint32_t> &deps = it->second;
            for (Set<uint32_t>::const_iterator dit = deps.begin(); dit != deps.end(); ++dit) {
                write<256>("    %s (%d)", Location::path(*dit).constData(), *dit);
            }
            if (isAborted())
                return;
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "symbols")) {
        matched = true;
        Scope<const SymbolMap&> scope = proj->lockSymbolsForRead();
        if (scope.isNull())
            return;
        const SymbolMap &map = scope.data();
        write(delimiter);
        write("symbols");
        write(delimiter);
        for (SymbolMap::const_iterator it = map.begin(); it != map.end(); ++it) {
            const Location loc = it->first;
            const CursorInfo ci = it->second;
            write(loc);
            write(ci);
            if (isAborted())
                return;
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "symbolnames")) {
        matched = true;
        Scope<const SymbolNameMap&> scope = proj->lockSymbolNamesForRead();
        if (scope.isNull())
            return;
        const SymbolNameMap &map = scope.data();
        write(delimiter);
        write("symbolnames");
        write(delimiter);
        for (SymbolNameMap::const_iterator it = map.begin(); it != map.end(); ++it) {
            write<128>("  %s", it->first.constData());
            const Set<Location> &locations = it->second;
            for (Set<Location>::const_iterator lit = locations.begin(); lit != locations.end(); ++lit) {
                const Location &loc = *lit;
                write<1024>("    %s", loc.key().constData());
            }
            if (isAborted())
                return;
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "fileinfos")) {
        matched = true;
        const SourceInformationMap map = proj->sources();
        write(delimiter);
        write("fileinfos");
        write(delimiter);
        for (SourceInformationMap::const_iterator it = map.begin(); it != map.end(); ++it) {
            for (int i=0; i<it->second.builds.size(); ++i) {
                write<512>("  %s: %s", Location::path(it->first).constData(), it->second.builds.at(i).compiler.constData(),
                           String::join(it->second.builds.at(i).args, " ").constData());
            }
        }
    }
}
