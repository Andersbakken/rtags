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

#include "StatusJob.h"
#include "CursorInfo.h"
#include "RTags.h"
#include "Server.h"
#include <clang-c/Index.h>
#include "Project.h"

const char *StatusJob::delimiter = "*********************************";
StatusJob::StatusJob(const QueryMessage &q, const std::shared_ptr<Project> &project)
    : Job(q, WriteUnfiltered|QuietJob, project), query(q.query())
{
}

void StatusJob::execute()
{
    bool matched = false;
    const char *alternatives = "fileids|watchedpaths|dependencies|symbols|symbolnames|sources";

    if (!strcasecmp(query.constData(), "fileids")) {
        matched = true;
        if (!write(delimiter) || !write("fileids") || !write(delimiter))
            return;
        const Hash<uint32_t, Path> paths = Location::idsToPaths();
        for (Hash<uint32_t, Path>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
            if (!write<256>("  %u: %s", it->first, it->second.constData()))
                return;
        }
        if (isAborted())
            return;
    }

    std::shared_ptr<Project> proj = project();
    if (!proj) {
        if (!matched)
            write(alternatives);
        return;
    }

    if (query.isEmpty() || !strcasecmp(query.constData(), "watchedpaths")) {
        matched = true;
        if (!write(delimiter) || !write("watchedpaths") || !write(delimiter))
            return;
        Set<Path> watched = proj->watchedPaths();
        if (!write("Indexer"))
            return;
        for (Set<Path>::const_iterator it = watched.begin(); it != watched.end(); ++it) {
            if (!write<256>("  %s", it->constData()))
                return;
        }
        if (proj->fileManager) {
            if (!write("FileManager"))
                return;
            watched = proj->fileManager->watchedPaths();
            for (Set<Path>::const_iterator it = watched.begin(); it != watched.end(); ++it) {
                if (!write<256>("  %s", it->constData()))
                    return;
            }
        }
        if (isAborted())
            return;
    }

    if (query.isEmpty() || !strcasecmp(query.constData(), "dependencies")) {
        matched = true;
        const DependencyMap map = proj->dependencies();
        if (!write(delimiter) || !write("dependencies") || !write(delimiter))
            return;
        DependencyMap depsReversed;

        for (DependencyMap::const_iterator it = map.begin(); it != map.end(); ++it) {
            if (!write<256>("  %s (%d) is depended on by", Location::path(it->first).constData(), it->first))
                return;
            const Set<uint32_t> &deps = it->second;
            for (Set<uint32_t>::const_iterator dit = deps.begin(); dit != deps.end(); ++dit) {
                if (!write<256>("    %s (%d)", Location::path(*dit).constData(), *dit))
                    return;
                depsReversed[*dit].insert(it->first);
            }
            if (isAborted())
                return;
        }
        for (DependencyMap::const_iterator it = depsReversed.begin(); it != depsReversed.end(); ++it) {
            write<256>("  %s (%d) depends on", Location::path(it->first).constData(), it->first);
            const Set<uint32_t> &deps = it->second;
            for (Set<uint32_t>::const_iterator dit = deps.begin(); dit != deps.end(); ++dit) {
                if (!write<256>("    %s (%d)", Location::path(*dit).constData(), *dit))
                    return;
            }
            if (isAborted())
                return;
        }
    }

    if (query.isEmpty() || !strcasecmp(query.constData(), "symbols")) {
        matched = true;
        const SymbolMap &map = proj->symbols();
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

    if (query.isEmpty() || !strcasecmp(query.constData(), "symbolnames")) {
        matched = true;
        const SymbolNameMap &map = proj->symbolNames();
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

    if (query.isEmpty() || !strcasecmp(query.constData(), "sources")) {
        matched = true;
        const SourceMap &map = proj->sources();
        if (!write(delimiter) || !write("sources") || !write(delimiter))
            return;
        for (SourceMap::const_iterator it = map.begin(); it != map.end(); ++it) {
            if (!write<512>("  %s: %s", it->second.sourceFile().constData(), it->second.toString().constData()))
                return;
        }
    }

    if (!matched)
        write<256>("rc -s %s", alternatives);
}
