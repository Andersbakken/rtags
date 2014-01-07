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
    // if (flags & (Aborted|Running|Remote)) {
    //     error() << "Restarting job" << sourceFile << "when state is" << String::format<8>("0x%x", flags);
    //     return true; // ### ???
    // }
    // assert(!(flags & (Aborted|Running)));
    assert(cpp);
    assert(!cpp->preprocessed.isEmpty());
    static const Path rp = Rct::executablePath().parentDir() + "rp";
    String stdinData;
    {
        Serializer serializer(stdinData);
        encode(serializer);
    }

    started = 0;
    assert(!process);
    process = new Process;
    if (!process->start(rp)) {
        error() << "Couldn't start rp" << process->errorString();
        return false;
    }

    flags |= Running;
    process->write(stdinData);
    return true;
}

bool IndexerJob::update(unsigned int f, const Source &s, const std::shared_ptr<Cpp> &c)
{
    assert(!(flags & Complete));

    if ((flags & (Running|Remote)) == Running) {
        abort();
    } else if (!(flags & (Aborted|Crashed))) { // still pending
        flags = f;
        source = s;
        assert(cpp);
        cpp = c;
        return true;
    }
    return false;
}

void IndexerJob::abort()
{
    assert(!(flags & Complete));
    if (process && flags & Running) { // only kill once
        process->kill();
        assert(!(flags & FromRemote)); // this is not handled
    }
    flags &= ~Running;
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

// void IndexerJob::onProcessFinished()
// {
//     assert(!port);
//     // error() << "PROCESS FINISHED" << source.sourceFile() << process->returnCode() << this;
//     ::error() << process->readAllStdOut();
//     ::error() << process->readAllStdErr();
//     if (process->returnCode() != 0) {
//         state = Crashed;
//         std::shared_ptr<Project> proj = Server::instance()->project(project);
//         if (proj && proj->state() == Project::Loaded) {
//             std::shared_ptr<IndexData> data(new IndexData(type));
//             data->key = source.key();
//             data->aborted = true;
//             EventLoop::SharedPtr loop = EventLoop::eventLoop();
//             assert(loop);
//             loop->callLater([proj, data]() { proj->onJobFinished(data); });
//         }
//     }
//     // ::error() << source.sourceFile() << "finished" << process->returnCode() << mWaiting << mTimer.elapsed() << "ms";
// }
