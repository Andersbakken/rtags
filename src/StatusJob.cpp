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

#include "StatusJob.h"

#include <stdint.h>
#include <strings.h>
#include <map>
#include <unordered_map>
#include <utility>

#include "CompilerManager.h"
#include "Project.h"
#include "RTags.h"
#include "Server.h"
#include "FileMap.h"
#include "IndexParseData.h"
#include "Location.h"
#include "QueryMessage.h"
#include "Source.h"
#include "Symbol.h"
#include "rct/Flags.h"
#include "rct/Hash.h"
#include "rct/List.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Set.h"

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
    const char *alternatives = "fileids|watchedpaths|dependencies|cursors|symbols|targets|symbolnames|sources|jobs|daemon|info|compilers|memory|project";

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
        out << "socketFile: " << opt.socketFile << '\n'
            << "dataDir: " << opt.dataDir << '\n'
            << "options: " << opt.options << '\n'
            << "jobCount: " << opt.jobCount << '\n'
            << "rpVisitFileTimeout: " << opt.rpVisitFileTimeout << '\n'
            << "rpIndexDataMessageTimeout: " << opt.rpIndexDataMessageTimeout << '\n'
            << "rpConnectTimeout: " << opt.rpConnectTimeout << '\n'
            << "rpConnectTimeout: " << opt.rpConnectTimeout << '\n'
            << "defaultArguments: " << opt.defaultArguments << '\n'
            << "includePaths: " << opt.includePaths << '\n'
            << "defines: " << opt.defines << '\n'
            << "ignoredCompilers: " << opt.ignoredCompilers;
        write(out);
    }

    if (query.isEmpty() || match("jobs")) {
        matched = true;
        if (!write(delimiter) || !write("jobs") || !write(delimiter))
            return 1;
        Server::instance()->dumpJobs(connection());
    }

    if (query.isEmpty() || match("daemon")) {
        matched = true;
        if (!write(delimiter) || !write("daemon") || !write(delimiter))
            return 1;
        Server::instance()->dumpDaemons(connection());
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
        Hash<Path, Flags<Project::WatchMode> > watched = proj->watchedPaths();
        auto watchModeToString = [](Flags<Project::WatchMode> mode) {
            List<String> ret;
            if (mode & Project::Watch_FileManager)
                ret << "filemanager";
            if (mode & Project::Watch_SourceFile)
                ret << "source";
            if (mode & Project::Watch_Dependency)
                ret << "dependency";
            if (mode & Project::Watch_CompileCommands)
                ret << "compilecommands";
            return String::join(ret, '|');
        };
        for (const auto &it : watched) {
            if (!write<256>("  %s (%s)", it.first.constData(), watchModeToString(it.second).constData())) {
                return 1;
            }
        }
        for (auto p : Server::instance()->projects()) {
            if (p.second != proj) {
                watched = p.second->watchedPaths();
                if (watched.empty())
                    continue;
                write<256>("Project: %s", p.first.c_str());
                for (const auto &it : watched) {
                    if (!write<256>("  %s (%s)", it.first.constData(), watchModeToString(it.second).constData())) {
                        return 1;
                    }
                }
            }
        }
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
                const Symbol c = symbols->valueAt(i);
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
                    write<1024>("      %s\t%s", t.location.toString(locationToStringFlags()).constData(),
                                t.kindSpelling().constData());
                }
                for (const auto &location : targets->valueAt(i)) {
                    write<1024>("    %s", location.toString(locationToStringFlags()).constData());
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
                for (Location loc : symNames->valueAt(i)) {
                    write<1024>("    %s", loc.toString().constData());
                }
                write("------------------------");
                if (isAborted())
                    return 1;
            }
        }
    }

    if (query.isEmpty() || match("sources")) {
        matched = true;
        if (!write(delimiter) || !write("sources") || !write(delimiter))
            return 1;
        proj->indexParseData().write([this](const String &str) { return write(str); });
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
            CompilerManager::applyToSource(source, CompilerManager::IncludeIncludePaths|CompilerManager::IncludeDefines);
            write(compiler);
            write("  Defines:");
            for (const auto &it : source.defines)
                write<512>("    %s", it.toString().constData());
            write("  Includepaths:");
            for (const auto &it : source.includePaths)
                write<512>("    %s", it.toString().constData());
            write(String());
        }
    }

    if (query.isEmpty() || match("memory")) {
        if (!write(delimiter) || !write("memory") || !write(delimiter))
            return 1;
        write(proj->estimateMemory());
        matched = true;
    }

    if (query.isEmpty() || match("project")) {
        if (!write(delimiter) || !write("project") || !write(delimiter))
            return 1;
        write(String::format<1024>("Path: %s", proj->path().constData()));
        bool first = true;
        for (const auto &info : proj->indexParseData().compileCommands) {
            if (first) {
                first = false;
                write("\nCompile commands:");
            }
            write(String::format<1024>("    File: %s\n"
                                       "    Last-Modified: %s (%llu)\n"
                                       "    Bytes written: %zu\n",
                                       Location::path(info.first).constData(),
                                       String::formatTime(info.second.lastModifiedMs / 1000).constData(),
                                       static_cast<unsigned long long>(info.second.lastModifiedMs),
                                       proj->bytesWritten()));

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
