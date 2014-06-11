#include "IndexerJob.h"
#include "Project.h"
#include <rct/Process.h>
#include <RTagsClang.h>
#include "Server.h"
#include "Unit.h"

uint64_t IndexerJob::nextId = 0;

IndexerJob::IndexerJob(const Path &p, const std::shared_ptr<Unit> &u)
    : destination(Server::instance()->options().socketFile),
      port(0), project(p), process(0), id(++nextId), started(0), unit(u)
{
    assert(unit);
    for (auto it = u->visited.begin(); it != u->visited.end(); ++it)
        visited.insert(it->second);
}

IndexerJob::IndexerJob()
    : port(0), process(0), id(0), started(0)
{
}

IndexerJob::~IndexerJob()
{
    delete process;
}

bool IndexerJob::launchProcess()
{
    assert(unit);
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

    unit->flags |= RunningLocal;

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

bool IndexerJob::update(const std::shared_ptr<Unit> &u)
{
    assert(u);
    // error() << "Updating" << s.sourceFile() << dumpFlags(flags);
    assert(!(u->flags & (CompleteLocal|CompleteRemote)));

    if (!(u->flags & (RunningLocal|Remote))) {
        unit = u;
        return true;
    }
    abort();
    return false;
}

void IndexerJob::abort()
{
    // error() << "Aborting job" << source.sourceFile() << !!process << dumpFlags(flags);
    if (process && unit->flags & RunningLocal) { // only kill once
        process->kill();
        assert(!(unit->flags & FromRemote)); // this is not handled
    }
    if (unit->flags & (CompleteRemote|CompleteLocal)) {
        error() << "Aborting a job that already is complete";
    }
    assert(!(unit->flags & (CompleteRemote|CompleteLocal)));
    unit->flags &= ~RunningLocal;
    unit->flags |= Aborted;
}

void IndexerJob::encode(Serializer &serializer)
{
    assert(unit);
    std::shared_ptr<Project> proj;
    if (!(unit->flags & FromRemote))
        proj = Server::instance()->project(project);
    const Server::Options &options = Server::instance()->options();
    const Source oldSource = unit->source;
    if (options.options & Server::Wall && unit->source.arguments.contains("-Werror")) {
        for (const auto &arg : options.defaultArguments) {
            if (arg != "-Wall")
                unit->source.arguments << arg;
        }
    } else {
        unit->source.arguments << options.defaultArguments;
    }
    for (const auto &inc : options.includePaths) {
        unit->source.includePaths << Source::Include(Source::Include::Type_Include, inc);
    }
    unit->source.defines << options.defines;
    serializer << static_cast<uint16_t>(RTags::DatabaseVersion)
               << destination << port << *unit << project
               << static_cast<uint32_t>(options.rpVisitFileTimeout)
               << static_cast<uint32_t>(options.rpIndexerMessageTimeout)
               << static_cast<uint32_t>(options.rpConnectTimeout)
               << static_cast<bool>(options.options & Server::SuspendRPOnCrash) << id;
    unit->source = oldSource;
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
