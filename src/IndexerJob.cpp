#include "IndexerJob.h"
#include "Project.h"
#include <rct/Process.h>
#include <RTagsClang.h>
#include "Server.h"

IndexerJob::IndexerJob(IndexType t, const Path &p, const Source &s, const String &cpp)
    : state(Pending), destination(Server::instance()->options().socketFile),
      port(0), type(t), project(p), source(s), preprocessed(cpp),
      sourceFile(s.sourceFile()), process(0)
{
}

IndexerJob::IndexerJob()
    : state(Pending), port(0), type(Invalid), process(0)
{
}

bool IndexerJob::startLocal()
{
    if (state == Aborted)
        return false;
    assert(state == Pending);
    state = Running;
    assert(!process);
    assert(!preprocessed.isEmpty());
    static const Path rp = Rct::executablePath().parentDir() + "rp";
    String stdinData;
    Serializer serializer(stdinData);
    if (!encode(serializer))
        return false;

    process = new Process;
    if (!port)
        process->finished().connect(std::bind(&IndexerJob::onProcessFinished, this));
    if (!process->start(rp)) {
        error() << "Couldn't start rp" << process->errorString();
        delete process;
        process = 0;
        return false;
    }
    process->write(stdinData);
    return true;
}

bool IndexerJob::update(IndexType t, const Source &s)
{
    switch (state) {
    case Aborted:
        break;
    case Running:
        abort();
        break;
    case Pending:
        type = t;
        source = s;
        return true;
    }
    return false;
}

void IndexerJob::abort()
{
    switch (state) {
    case Aborted:
    case Pending:
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
    std::shared_ptr<Project> proj = Server::instance()->project(project);
    if (!proj)
        return false;
    const Server::Options &options = Server::instance()->options();
    Source copy = source;
    copy.arguments << options.defaultArguments;
    serializer << destination << port << sourceFile
               << copy << preprocessed << project
               << static_cast<uint8_t>(type) << options.rpVisitFileTimeout
               << options.rpIndexerMessageTimeout
               << proj->visitedFiles();
    return true;
}

void IndexerJob::decode(Deserializer &deserializer, Hash<Path, uint32_t> &blockedFiles)
{
    uint8_t t;
    int ignored; // timeouts
    deserializer >> destination >> port >> sourceFile
                 >> source >> preprocessed >> project
                 >> t >> ignored >> ignored >> blockedFiles;
    type = static_cast<IndexType>(t);
}

void IndexerJob::onProcessFinished()
{
    assert(!port);
    // error() << "PROCESS FINISHED" << source.sourceFile() << process->returnCode() << this;
    ::error() << process->readAllStdOut();
    ::error() << process->readAllStdErr();
    if (process->returnCode() == -1) {
        std::shared_ptr<Project> proj = Server::instance()->project(project);
        if (proj && proj->state() == Project::Loaded) {
            std::shared_ptr<IndexData> data(new IndexData(type));
            data->fileId = source.fileId;
            data->aborted = true;
            EventLoop::SharedPtr loop = EventLoop::eventLoop();
            assert(loop);
            loop->callLater([proj, data]() { proj->onJobFinished(data); });
        }
    }
    // ::error() << source.sourceFile() << "finished" << process->returnCode() << mWaiting << mTimer.elapsed() << "ms";
}
