/* This file is part of RTags (http://rtags.net).

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
#include "RTags.h"
#include "Server.h"
#include <clang-c/Index.h>
#include "Project.h"
#include "CompilerManager.h"
#include "JobScheduler.h"

const char *StatusJob::delimiter = "*********************************";
StatusJob::StatusJob(const std::shared_ptr<QueryMessage> &q, const std::shared_ptr<Project> &project)
    : QueryJob(q, project, WriteUnfiltered|QuietJob), query(q->query())
{
}

int StatusJob::execute()
{
    auto match = [this](const char *name) {
        return !strncasecmp(query.constData(), name, query.size());
    };
    bool matched = false;
    const char *alternatives = "fileids|watchedpaths|dependencies|cursors|symbols|targets|symbolnames|sources|jobs|info|compilers|declarations|headererrors";

    if (match("fileids")) {
        matched = true;
        if (!write(delimiter) || !write("fileids") || !write(delimiter))
            return 1;
        const Hash<uint32_t, Path> paths = Location::idsToPaths();
        for (Hash<uint32_t, Path>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
            if (!write<256>("  %u: %s", it->first, it->second.constData()))
                return 1;
        }
        if (isAborted())
            return 1;
    }

    if (match("headererrors")) {
        matched = true;
        if (!write(delimiter) || !write("headererrors") || !write(delimiter))
            return 1;
        for (auto err : Server::instance()->jobScheduler()->headerErrors()) {
            if (!write(Location::path(err)))
                return 1;
        }
        if (isAborted())
            return 1;
    }

    if (query.isEmpty() || match("info")) {
        matched = true;
        if (!write(delimiter) || !write("info") || !write(delimiter))
            return 1;
        String out;
        Log log(&out);
#ifdef NDEBUG
        out << "Running a release build\n";
#else
        out << "Running a debug build\n";
#endif
        const Server::Options &opt = Server::instance()->options();
        out << "socketFile" << opt.socketFile << '\n'
            << "dataDir" << opt.dataDir << '\n'
            << "options" << opt.options
            << "jobCount" << opt.jobCount << '\n'
            << "rpVisitFileTimeout" << opt.rpVisitFileTimeout << '\n'
            << "rpIndexDataMessageTimeout" << opt.rpIndexDataMessageTimeout << '\n'
            << "rpConnectTimeout" << opt.rpConnectTimeout << '\n'
            << "rpConnectTimeout" << opt.rpConnectTimeout << '\n'
            << "threadStackSize" << opt.threadStackSize << '\n'
            << "defaultArguments" << opt.defaultArguments << '\n'
            << "includePaths" << opt.includePaths << '\n'
            << "defines" << opt.defines << '\n'
            << "ignoredCompilers" << opt.ignoredCompilers;
        write(out);
    }


    std::shared_ptr<Project> proj = project();
    if (!proj) {
        if (!matched)
            write(alternatives);
        return matched ? 0 : 1;
    }

    if (query.isEmpty() || match("watchedpaths")) {
        matched = true;
        if (!write(delimiter) || !write("watchedpaths") || !write(delimiter))
            return 1;
        Set<Path> watched = proj->watchedPaths();
        if (!write("Indexer"))
            return 1;
        for (Set<Path>::const_iterator it = watched.begin(); it != watched.end(); ++it) {
            if (!write<256>("  %s", it->constData()))
                return 1;
        }
        if (proj->fileManager) {
            if (!write("FileManager"))
                return 1;
            watched = proj->fileManager->watchedPaths();
            for (Set<Path>::const_iterator it = watched.begin(); it != watched.end(); ++it) {
                if (!write<256>("  %s", it->constData()))
                    return 1;
            }
        }
        if (isAborted())
            return 1;
    }

    const Dependencies &deps = proj->dependencies();
    if (query.isEmpty() || match("dependencies")) {
        matched = true;
        if (!write(delimiter) || !write("dependencies") || !write(delimiter))
            return 1;

        for (auto it : deps) {
            write(proj->dumpDependencies(it.first));
        }
        if (isAborted())
            return 1;
    }

    if (query.isEmpty() || match("symbols") || match("cursors")) {
        matched = true;
        write(delimiter);
        write("symbols");
        write(delimiter);

        for (const auto &dep : deps) {
            auto symbols = proj->openSymbols(dep.first);
            if (!symbols)
                continue;
            const int count = symbols->count();
            for (int i=0; i<count; ++i) {
                const Location loc = symbols->keyAt(i);
                const Symbol c = symbols->valueAt(i);
                write(loc);
                write(c);
                write("------------------------");
                if (isAborted())
                    return 1;
            }
        }
    }

    if (query.isEmpty() || match("targets")) {
        matched = true;
        write(delimiter);
        write("targets");
        write(delimiter);
        for (const auto &dep : deps) {
            auto targets = proj->openTargets(dep.first);
            if (!targets)
                continue;
            const int count = targets->count();
            for (int i=0; i<count; ++i) {
                const String usr = targets->keyAt(i);
                write<128>("  %s", usr.constData());
                for (const auto &t : proj->findByUsr(usr, dep.first, Project::ArgDependsOn)) {
                    write<1024>("      %s\t%s", t.location.key(keyFlags()).constData(),
                                t.kindSpelling().constData());
                }
                for (const auto &location : targets->valueAt(i)) {
                    write<1024>("    %s", location.key(keyFlags()).constData());
                }
                write("------------------------");
                if (isAborted())
                    return 1;
            }
        }
    }

    if (query.isEmpty() || match("symbolnames")) {
        matched = true;
        write(delimiter);
        write("symbolnames");
        write(delimiter);
        for (const auto &dep : deps) {
            auto symNames = proj->openSymbolNames(dep.first);
            if (!symNames)
                continue;
            const int count = symNames->count();
            for (int i=0; i<count; ++i) {
                write<128>("  %s", symNames->keyAt(i).constData());
                for (const Location &loc : symNames->valueAt(i)) {
                    write<1024>("    %s", loc.key().constData());
                }
                write("------------------------");
                if (isAborted())
                    return 1;
            }
        }
    }

    if (query.isEmpty() || match("sources")) {
        matched = true;
        const Sources &map = proj->sources();
        if (!write(delimiter) || !write("sources") || !write(delimiter))
            return 1;
        for (Sources::const_iterator it = map.begin(); it != map.end(); ++it) {
            if (!write<512>("  %s: %s", it->second.sourceFile().constData(), it->second.toString().constData()))
                return 1;
        }
    }

    if (query.isEmpty() || match("jobs")) {
        matched = true;
        if (!write(delimiter) || !write("jobs") || !write(delimiter))
            return 1;
        Server::instance()->dumpJobs(connection());
    }

    if (query.isEmpty() || match("compilers")) {
        matched = true;
        if (!write(delimiter) || !write("compilers") || !write(delimiter))
            return 1;
        Source source;
        for (const Path &compiler : CompilerManager::compilers()) {
            source.compilerId = Location::insertFile(compiler);
            source.defines.clear();
            source.includePaths.clear();
            CompilerManager::applyToSource(source, true, true);
            write(compiler);
            write("  Defines:");
            for (const auto &it : source.defines)
                write<512>("    %s", it.toString().constData());
            write("  Includepaths:");
            for (const auto &it : source.includePaths)
                write<512>("    %s", it.toString().constData());
            write("");
        }
    }

    if (query.isEmpty() || match("declarations")) {
        for (const auto &it : proj->declarations()) {
            write(it.first);
            for (uint32_t file : it.second) {
                write<128>("  %s", Location::path(file).constData());
            }
        }
        matched = true;
    }

    if (!matched) {
        write<256>("rc -s %s", alternatives);
        return 1;
    } else {
        return 0;
    }
}
