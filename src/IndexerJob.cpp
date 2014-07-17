#include "IndexerJob.h"
#include "Project.h"
#include <rct/Process.h>
#include <RTagsClang.h>
#include "Server.h"
#include "CompilerManager.h"

IndexerJob::IndexerJob(const Source &s,
                       uint32_t f,
                       const Path &p,
                       const UnsavedFiles &u,
                       const Set<uint32_t> &d)
    : destination(Server::instance()->options().socketFile),
      source(s), sourceFile(s.sourceFile()), flags(f),
      project(p), unsavedFiles(u), dirty(d), process(0), started(0)
{
}

IndexerJob::IndexerJob()
    : process(0), started(0)
{
}

bool IndexerJob::launchProcess()
{
    static Path rp;
    if (rp.isEmpty()) {
        rp = Rct::executablePath().parentDir() + "rp";
        if (!rp.exists()) {
            rp = Rct::executablePath();
            rp.resolve();
            rp = rp.parentDir() + "rp";
        }
    }

    started = 0;
    assert(!process);
    process = new Process;
    if (!process->start(rp)) {
        error() << "Couldn't start rp" << rp << process->errorString();
        return false;
    }

    flags |= Running;

    {
        String stdinData;
        {
            Serializer serializer(stdinData);
            encode(serializer);
        }
        const int size = stdinData.size();
        String header;
        header.resize(sizeof(size));
        *reinterpret_cast<int*>(&header[0]) = size;
        process->write(header);
        process->write(stdinData);
        // error() << "Startingprocess" << (header.size() + stdinData.size()) << sourceFile;
    }
    return true;
}

bool IndexerJob::update(const Source &s, uint32_t f)
{
    // error() << "Updating" << s.sourceFile() << dumpFlags(flags);
    assert(!(flags & Complete));

    if (!(flags & Running)) {
        source = s;
        assert(sourceFile == source.sourceFile());
        flags = f;
        return true;
    }
    abort();
    return false;
}

void IndexerJob::abort()
{
    // error() << "Aborting job" << source.sourceFile() << !!process << dumpFlags(flags);
    if (process && flags & Running) { // only kill once
        process->kill();
    }
    if (flags & Complete) {
        error() << "Aborting a job that already is complete";
    }
    assert(!(flags & Complete));
    flags &= ~Running;
    flags |= Aborted;
}

void IndexerJob::encode(Serializer &serializer) const
{
    std::shared_ptr<Project> proj = Server::instance()->project(project);
    const Server::Options &options = Server::instance()->options();
    Source copy = source;
    if (options.options & Server::Wall && source.arguments.contains("-Werror")) {
        for (const auto &arg : options.defaultArguments) {
            if (arg != "-Wall")
                copy.arguments << arg;
        }
    } else {
        copy.arguments << options.defaultArguments;
    }

    if (!(options.options & Server::AllowPedantic)) {
        const int idx = copy.arguments.indexOf("-Wpedantic");
        if (idx != -1) {
            copy.arguments.removeAt(idx);
        }
    }

    CompilerManager::data(copy.compiler(), 0, &copy.includePaths);

    for (const auto &inc : options.includePaths) {
        copy.includePaths << Source::Include(Source::Include::Type_Include, inc);
    }
    copy.defines << options.defines;
    assert(!sourceFile.isEmpty());
    serializer << static_cast<uint16_t>(RTags::DatabaseVersion)
               << destination << project << copy << sourceFile << flags
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
    if (flags & HighPriority) {
        ret += "HighPriority";
    }

    return String::join(ret, ", ");
}
