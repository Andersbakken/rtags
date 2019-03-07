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

#include "RPVehicle.h"
#include "Server.h"
#include "Project.h"

RPProcess::RPProcess()
{
    Process::readyReadStdOut().connect([this](Process *) {
        Vehicle::readyReadStdOut()(shared_from_this());
    });
    Process::finished().connect([this](Process *) {
        Vehicle::finished()(shared_from_this());
    });
}

void RPProcess::kill()
{
    Process::kill();
}

String RPProcess::readAllStdOut()
{
    return Process::readAllStdOut();
}

String RPProcess::readAllStdErr()
{
    return Process::readAllStdErr();
}

String RPProcess::errorString() const
{
    return Process::errorString();
}

unsigned long long RPProcess::id() const
{
    return Process::pid();
}

int RPProcess::returnCode() const
{
    return Process::returnCode();
}

bool RPProcess::start(const std::shared_ptr<IndexerJob> &job)
{
    List<String> arguments;
    arguments << "--priority" << String::number(job->priority());

    for (int i=logLevel().toInt(); i>0; --i)
        arguments << "-v";

    if (Server::instance()->options().options & Server::RPLogToSyslog) {
        arguments << "--log-to-syslog";
    }

    if (!Process::start(Server::instance()->options().rp, arguments)) {
        error() << "Failed to start rp";
        return false;
    }

    Process::write(job->encode());
    return true;
}

static unsigned long long sId = 0;
RPThread::RPThread()
    : mId(++sId)
{
}

void RPThread::kill()
{
    {
        std::unique_lock<std::mutex> lock(mMutex);
        mKilled = true;
    }
    waitForState(Finished);
}

void RPThread::run()
{
    Config config;
    config.sourceFile = mJob->sources.front().sourceFile();
    config.sources = mJob->sources;
    config.project = mJob->project;
    const Server::Options &options = Server::instance()->options();
    config.dataDir = options.dataDir;
    config.unsavedFiles = mJob->unsavedFiles;
    config.indexerJobFlags = mJob->flags;
    config.serverOpts = options.options;
    config.debugLocations = options.debugLocations;
    config.id = mJob->id;
    exec(std::move(config));
    std::shared_ptr<Vehicle> vehicle = shared_from_this();
    EventLoop::mainEventLoop()->callLater([vehicle]() {
        vehicle->finished()(vehicle);
    });
}

String RPThread::readAllStdOut()
{
    return String();
}

String RPThread::readAllStdErr()
{
    return String();
}

String RPThread::errorString() const
{
    return mErrorString;
}

unsigned long long RPThread::id() const
{
    return mId;
}

int RPThread::returnCode() const
{
    return 0;
}

bool RPThread::start(const std::shared_ptr<IndexerJob> &job)
{
    mJob = job;
    mProject = Server::instance()->project(job->project);
    mSourceFileId = job->fileId();
    if (!mProject) {
        mErrorString = "Can't find project " + job->project;
        return false;
    }
    assert(Server::instance());
    assert(Server::instance()->threadPool());
    Server::instance()->threadPool()->start(std::static_pointer_cast<RPThread>(shared_from_this()));
    return true;
}

bool RPThread::interrupt()
{
    std::unique_lock<std::mutex> lock(mMutex);
    return mKilled;
}

Location RPThread::createLocation(const Path &sourceFile, unsigned int line, unsigned int col, bool *blockedPtr)
{
    static_assert(sizeof(unsigned int) == sizeof(uint32_t), "We should deal with it if unsigned int is 64-bit");
    const Path resolved = sourceFile.resolved();
    const uint32_t fileId = Location::insertFile(resolved);
    Location location(fileId, line, col);
    if (mProject->visitFile(fileId, resolved, mSourceFileId)) {
        if (blockedPtr)
            *blockedPtr = false;
        indexDataMessage().files()[fileId] |= IndexDataMessage::Visited;
    } else if (blockedPtr) {
        *blockedPtr = true;
    }
    return Location(fileId, line, col);
}

bool RPThread::send(const std::shared_ptr<IndexDataMessage> &message)
{
    EventLoop::mainEventLoop()->callLater([message]() {
        Server::instance()->onNewMessage(message, std::shared_ptr<Connection>());
    });
    return true;
}
