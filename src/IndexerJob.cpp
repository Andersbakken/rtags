#include "IndexerJob.h"
#include "Project.h"
#include <rct/Process.h>
#include <RTagsClang.h>
#include "Server.h"
#include "Cpp.h"

uint64_t IndexerJob::nextId = 0;

IndexerJob::IndexerJob(uint32_t f, const Path &p, const Source &s, const std::shared_ptr<Cpp> &c)
    : flags(f), destination(Server::instance()->options().socketFile),
      port(0), project(p), source(s), sourceFile(s.sourceFile()),
      process(0), id(++nextId), started(0), cpp(c)
{
    for (auto it = c->visited.begin(); it != c->visited.end(); ++it)
        visited.insert(it->second);
    assert(cpp);
}

IndexerJob::IndexerJob()
    : flags(0), port(0), process(0), id(0), started(0)
{
}

IndexerJob::~IndexerJob()
{
    delete process;
}

bool IndexerJob::launchProcess()
{
    assert(cpp);
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

    flags |= RunningLocal;

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
        // error() << "Startingprocess" << (packet.size() + stdinData.size()) << sourceFile;
    }
    return true;
}

bool IndexerJob::update(unsigned int f, const Source &s, const std::shared_ptr<Cpp> &c)
{
    // error() << "Updating" << s.sourceFile() << dumpFlags(flags);
    assert(!(flags & (CompleteLocal|CompleteRemote)));

    if (!(flags & (RunningLocal|Remote))) {
        flags = f;
        source = s;
        assert(cpp);
        cpp = c;
        return true;
    }
    abort();
    return false;
}

void IndexerJob::abort()
{
    // error() << "Aborting job" << source.sourceFile() << !!process << dumpFlags(flags);
    if (process && flags & RunningLocal) { // only kill once
        process->kill();
        assert(!(flags & FromRemote)); // this is not handled
    }
    if (flags & (CompleteRemote|CompleteLocal)) {
        error() << "Aborting a job that already is complete";
    }
    assert(!(flags & (CompleteRemote|CompleteLocal)));
    flags &= ~RunningLocal;
    flags |= Aborted;
}

void IndexerJob::encode(Serializer &serializer)
{
    std::shared_ptr<Project> proj;
    if (!(flags & FromRemote))
        proj = Server::instance()->project(project);
    const Server::Options &options = Server::instance()->options();
    Source copy = source;
    copy.arguments << options.defaultArguments;
    copy.includePaths << options.includePaths;
    copy.defines << options.defines;
    assert(cpp);
    serializer << static_cast<uint16_t>(ProtocolVersion)
               << destination << port << sourceFile
               << copy << *cpp << project << flags
               << static_cast<uint32_t>(options.rpVisitFileTimeout)
               << static_cast<uint32_t>(options.rpIndexerMessageTimeout)
               << static_cast<uint32_t>(options.rpConnectTimeout)
               << static_cast<bool>(options.options & Server::SuspendRPOnCrash) << id;
    if (blockedFiles.isEmpty() && proj) {
        proj->encodeVisitedFiles(serializer);
    } else {
        serializer << blockedFiles;
    }
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
    if (flags & FromRemote) {
        ret += "FromRemote";
    }
    if (flags & Remote) {
        ret += "Remote";
    }
    if (flags & Rescheduled) {
        ret += "Rescheduled";
    }
    if (flags & RunningLocal) {
        ret += "RunningLocal";
    }
    if (flags & Crashed) {
        ret += "Crashed";
    }
    if (flags & Aborted) {
        ret += "Aborted";
    }
    if (flags & CompleteLocal) {
        ret += "CompleteLocal";
    }
    if (flags & CompleteRemote) {
        ret += "CompleteRemote";
    }
    return String::join(ret, ", ");
}
