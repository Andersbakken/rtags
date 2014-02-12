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
    String stdinData;
    {
        Serializer serializer(stdinData);
        encode(serializer);
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
        const int size = stdinData.size();
        String packet;
        packet.resize(sizeof(size));
        *reinterpret_cast<int*>(&packet[0]) = size;
        process->write(packet);
        process->write(stdinData);
    }
    return true;
}

bool IndexerJob::update(unsigned int f, const Source &s, const std::shared_ptr<Cpp> &c)
{
    assert(!(flags & (CompleteLocal|CompleteRemote)));

    if (!flags & (RunningLocal|Remote)) {
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
    if (process && flags & RunningLocal) { // only kill once
        process->kill();
        assert(!(flags & FromRemote)); // this is not handled
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
    assert(cpp);
    serializer << destination << port << sourceFile
               << copy << *cpp << project
               << flags << options.rpVisitFileTimeout
               << options.rpIndexerMessageTimeout << options.rpConnectTimeout
               << id << (blockedFiles.isEmpty() && proj ? proj->visitedFiles() : blockedFiles);
}

void IndexerJob::decode(Deserializer &deserializer, Hash<Path, uint32_t> &blockedFiles)
{
    int ignored; // timeouts
    assert(!cpp);
    cpp.reset(new Cpp);
    deserializer >> destination >> port >> sourceFile
                 >> source >> *cpp >> project
                 >> flags >> ignored >> ignored >> id
                 >> blockedFiles;
}
