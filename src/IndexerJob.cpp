#include "IndexerJob.h"
#include "Project.h"
#include <rct/Process.h>
#include <RTagsClang.h>
#include "Server.h"
#include "Cpp.h"

uint64_t IndexerJob::nextId = 0;

IndexerJob::IndexerJob(IndexType t, const Path &p, const Source &s, const std::shared_ptr<Cpp> &c)
    : state(Pending), destination(Server::instance()->options().socketFile),
      port(0), type(t), project(p), source(s), sourceFile(s.sourceFile()),
      process(0), id(++nextId), started(0), cpp(c)
{
    for (auto it = c->visited.begin(); it != c->visited.end(); ++it)
        visited.insert(it->second);
    assert(cpp);
}

IndexerJob::IndexerJob()
    : state(Pending), port(0), type(Invalid), process(0), id(0), started(0)
{
}

bool IndexerJob::startLocal()
{
    if (state == Aborted)
        return false;
    if (state == Running)
        error() << "About to die" << sourceFile;
    assert(state != Running);
    assert(cpp);
    assert(!process);
    assert(!cpp->preprocessed.isEmpty());
    static const Path rp = Rct::executablePath().parentDir() + "rp";
    String stdinData;
    Serializer serializer(stdinData);
    if (!encode(serializer))
        return false;

    started = 0;
    process = new Process;
    if (!port)
        process->finished().connect(std::bind(&IndexerJob::onProcessFinished, this));
    if (!process->start(rp)) {
        error() << "Couldn't start rp" << process->errorString();
        delete process;
        process = 0;
        state = Crashed;
        return false;
    }
    state = Running;
    process->write(stdinData);
    return true;
}

bool IndexerJob::update(IndexType t, const Source &s, const std::shared_ptr<Cpp> &c)
{
    switch (state) {
    case Aborted:
    case Crashed:
        break;
    case Complete:
        // this shouldn't happen right?
        assert(0);
        break;
    case Running:
        abort();
        break;
    case Pending:
        type = t;
        source = s;
        assert(cpp);
        cpp = c;
        return true;
    }
    return false;
}

void IndexerJob::abort()
{
    switch (state) {
    case Complete:
        // ### this should happen right?
        assert(0);
        break;
    case Aborted:
    case Pending:
    case Crashed:
        break;
    case Running:
        if (process)
            process->kill();
        break;
    }
    state = Aborted;
}

bool IndexerJob::encode(Serializer &serializer)
{
    std::shared_ptr<Project> proj;
    if (type != Remote) {
        proj = Server::instance()->project(project);
        if (!proj)
            return false;
    }
    const Server::Options &options = Server::instance()->options();
    Source copy = source;
    copy.arguments << options.defaultArguments;
    assert(cpp);
    serializer << destination << port << sourceFile
               << copy << *cpp << project
               << static_cast<uint8_t>(type) << options.rpVisitFileTimeout
               << options.rpIndexerMessageTimeout
               << id << (proj ? proj->visitedFiles() : blockedFiles);
    return true;
}

void IndexerJob::decode(Deserializer &deserializer, Hash<Path, uint32_t> &blockedFiles)
{
    uint8_t t;
    int ignored; // timeouts
    assert(!cpp);
    cpp.reset(new Cpp);
    deserializer >> destination >> port >> sourceFile
                 >> source >> *cpp >> project
                 >> t >> ignored >> ignored >> id
                 >> blockedFiles;
    type = static_cast<IndexType>(t);
}

void IndexerJob::onProcessFinished()
{
    assert(!port);
    // error() << "PROCESS FINISHED" << source.sourceFile() << process->returnCode() << this;
    ::error() << process->readAllStdOut();
    ::error() << process->readAllStdErr();
    if (process->returnCode() != 0) {
        state = Crashed;
        std::shared_ptr<Project> proj = Server::instance()->project(project);
        if (proj && proj->state() == Project::Loaded) {
            std::shared_ptr<IndexData> data(new IndexData(type));
            data->key = source.key();
            data->aborted = true;
            EventLoop::SharedPtr loop = EventLoop::eventLoop();
            assert(loop);
            loop->callLater([proj, data]() { proj->onJobFinished(data); });
        }
    }
    // ::error() << source.sourceFile() << "finished" << process->returnCode() << mWaiting << mTimer.elapsed() << "ms";
}
