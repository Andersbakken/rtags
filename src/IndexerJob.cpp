#include "IndexerJob.h"
#include "Project.h"
#include <rct/Process.h>
#include <RTagsClang.h>
#include "Server.h"
#include "CompilerManager.h"

uint64_t IndexerJob::sNextId = 1;
IndexerJob::IndexerJob(const Source &s,
                       uint32_t f,
                       const Path &p,
                       const UnsavedFiles &u,
                       const Set<uint32_t> &d)
    : id(sNextId++), source(s), sourceFile(s.sourceFile()), flags(f),
      project(p), unsavedFiles(u), dirty(d), crashCount(0)
{
}

String IndexerJob::encode() const
{
    String ret;
    {
        Serializer serializer(ret);
        serializer << static_cast<int>(0); // for size
        std::shared_ptr<Project> proj = Server::instance()->project(project);
        const Server::Options &options = Server::instance()->options();
        Source copy = source;
        if (options.flag(Server::Wall) && source.arguments.contains("-Werror")) {
            for (const auto &arg : options.defaultArguments) {
                if (arg != "-Wall")
                    copy.arguments << arg;
            }
        } else {
            copy.arguments << options.defaultArguments;
        }

        if (!options.flag(Server::AllowPedantic)) {
            const int idx = copy.arguments.indexOf("-Wpedantic");
            if (idx != -1) {
                copy.arguments.removeAt(idx);
            }
        }

        if (options.flag(Server::EnableCompilerManager))
            CompilerManager::applyToSource(copy, false, true);

        for (const String &blocked : options.blockedArguments) {
            if (blocked.endsWith("=")) {
                const String arg = blocked.left(blocked.size() - 1);
                const int idx = copy.arguments.indexOf(arg);
                if (idx != -1) {
                    const int count = idx + 1 < copy.arguments.size() ? 2 : 1;
                    // error() << "removed args" << copy.arguments.at(idx)
                    //         << copy.arguments.at(idx + count - 1);
                    copy.arguments.remove(idx, count);
                }
            } else {
                const int idx = copy.arguments.indexOf(blocked);
                if (idx != -1) {
                    // error() << "removed arg" << copy.arguments.at(idx);
                    copy.arguments.removeAt(idx);
                }
            }
        }

        for (const auto &inc : options.includePaths) {
            copy.includePaths << inc;
        }
        copy.defines << options.defines;
        if (!(options.flag(Server::EnableNDEBUG))) {
            copy.defines.remove(Source::Define("NDEBUG"));
        }
        assert(!sourceFile.isEmpty());
        serializer << static_cast<uint16_t>(RTags::DatabaseVersion)
                   << id << options.socketFile
                   << (options.astCache > 0 ? options.dataDir + "astcache/" : String())
                   << project << copy << sourceFile << flags
                   << static_cast<uint32_t>(options.rpVisitFileTimeout)
                   << static_cast<uint32_t>(options.rpIndexerMessageTimeout)
                   << static_cast<uint32_t>(options.rpConnectTimeout)
                   << static_cast<int32_t>(options.rpNiceValue)
                   << static_cast<bool>(options.options & Server::SuspendRPOnCrash)
                   << unsavedFiles << static_cast<uint32_t>(dirty.size());
        for (uint32_t fileId : dirty) {
            serializer << Location::path(fileId);
        }
        assert(proj);
        proj->encodeVisitedFiles(serializer);
    }
    *reinterpret_cast<int*>(&ret[0]) = ret.size() - sizeof(int);
    return ret;
}

String IndexerJob::dumpFlags(unsigned int flags)
{
    List<String> ret;
    if (flags & Dirty) {
        ret += "Dirty";
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
