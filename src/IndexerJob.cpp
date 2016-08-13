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

#include "IndexerJob.h"

#include "CompilerManager.h"
#include "Project.h"
#include "rct/Process.h"
#include "RTags.h"
#include "Server.h"
#include "RTagsVersion.h"

uint64_t IndexerJob::sNextId = 1;
IndexerJob::IndexerJob(const Source &s,
                       Flags<Flag> f,
                       const std::shared_ptr<Project> &p,
                       const UnsavedFiles &u)
    : id(0), source(s), sourceFile(s.sourceFile()), flags(f),
      project(p->path()), priority(0), unsavedFiles(u), crashCount(0)
{
    acquireId();
    if (flags & Dirty) {
        ++priority;
    } else if (flags & Reindex) {
        priority += 4;
    }
    Server *server = Server::instance();
    assert(server);
    if (server->isActiveBuffer(source.fileId)) {
        priority += 8;
    } else if (DependencyNode *node = p->dependencyNode(source.fileId)) {
        Set<DependencyNode*> seen;
        seen.insert(node);
        std::function<bool(const DependencyNode *node)> func = [&](const DependencyNode *node) {
            for (const auto &inc : node->includes) {
                if (seen.insert(inc.second)
                    && !Location::path(node->fileId).isSystem()
                    && (server->isActiveBuffer(node->fileId) || func(inc.second))) {
                    return true;
                }
            }
            return false;
        };
        if (func(node))
            priority += 2;
    }
    visited.insert(s.fileId);
}

IndexerJob::~IndexerJob()
{
    destroyed(this);
}

void IndexerJob::acquireId()
{
    id = sNextId++;
}

String IndexerJob::encode() const
{
    String ret;
    {
        Serializer serializer(ret);
        serializer.write("1234", sizeof(int)); // for size
        std::shared_ptr<Project> proj = Server::instance()->project(project);
        const Server::Options &options = Server::instance()->options();
        Source copy = source;
        if (!(options.options & Server::AllowWErrorAndWFatalErrors)) {
            int idx = source.arguments.indexOf("-Werror");
            if (idx != -1)
                copy.arguments.removeAt(idx);
            idx = source.arguments.indexOf("-Wfatal-error");
            if (idx != -1)
                copy.arguments.removeAt(idx);
        }
        copy.arguments << options.defaultArguments;

        if (!(options.options & Server::AllowPedantic)) {
            const int idx = copy.arguments.indexOf("-Wpedantic");
            if (idx != -1) {
                copy.arguments.removeAt(idx);
            }
        }

        if (options.options & Server::EnableCompilerManager) {
            CompilerManager::applyToSource(copy, CompilerManager::IncludeIncludePaths);
        }

        for (const String &blocked : options.blockedArguments) {
            if (blocked.endsWith("=")) {
                size_t i = 0;
                while (i<copy.arguments.size()) {
                    if (copy.arguments.at(i).startsWith(blocked)) {
                        // error() << "Removing" << copy.arguments.at(i);
                        copy.arguments.remove(i, 1);
                    } else if (!strncmp(blocked.constData(), copy.arguments.at(i).constData(), blocked.size() - 1)) {
                        const size_t count = (i + 1 < copy.arguments.size()) ? 2 : 1;
                        // error() << "Removing" << copy.arguments.mid(i, count);
                        copy.arguments.remove(i, count);
                    } else {
                        ++i;
                    }
                }
            } else {
                copy.arguments.remove(blocked);
            }
        }

        for (const auto &inc : options.includePaths) {
            copy.includePaths << inc;
        }
        if (Server::instance()->options().options & Server::PCHEnabled)
            proj->fixPCH(copy);

        copy.defines << options.defines;
        if (!(options.options & Server::EnableNDEBUG)) {
            copy.defines.remove(Source::Define("NDEBUG"));
        }
        assert(!sourceFile.isEmpty());
        serializer << static_cast<uint16_t>(RTags::DatabaseVersion)
                   << options.sandboxRoot
                   << id
                   << options.socketFile
                   << project;
        copy.encode(serializer, Source::IgnoreSandbox);
        serializer << sourceFile
                   << flags
                   << static_cast<uint32_t>(options.rpVisitFileTimeout)
                   << static_cast<uint32_t>(options.rpIndexDataMessageTimeout)
                   << static_cast<uint32_t>(options.rpConnectTimeout)
                   << static_cast<uint32_t>(options.rpConnectAttempts)
                   << static_cast<int32_t>(options.rpNiceValue)
                   << options.options
                   << unsavedFiles
                   << options.dataDir
                   << options.debugLocations;
        assert(proj);
        proj->encodeVisitedFiles(serializer);
    }
    const uint32_t size = ret.size() - sizeof(int);
    memcpy(&ret[0], &size, sizeof(size));
    return ret;
}

String IndexerJob::dumpFlags(Flags<Flag> flags)
{
    List<String> ret;
    if (flags & Dirty) {
        ret += "Dirty";
    }
    if (flags & Reindex) {
        ret += "Reindex";
    }
    if (flags & Compile) {
        ret += "Compile";
    }
    if (flags & Running) {
        ret += "Running";
    }
    if (flags & Crashed) {
        ret += "Crashed";
    }
    if (flags & Aborted) {
        ret += "Aborted";
    }
    if (flags & Complete) {
        ret += "Complete";
    }

    return String::join(ret, ", ");
}
