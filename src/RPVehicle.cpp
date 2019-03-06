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

RPProcess::RPProcess()
{
    Process::readyReadStdOut().connect([this](Process *) {
        mReadyReadStdOut(this);
    });
    Process::finished().connect([this](Process *) {
        mFinished(this);
    });
}

void RPProcess::kill()
{
    Process::kill();
}

Signal<std::function<void(Vehicle *)>> &RPProcess::readyReadStdOut()
{
    return mReadyReadStdOut;
}

Signal<std::function<void(Vehicle *)>> &RPProcess::finished()
{
    return mFinished;
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

int RPProcess::id() const
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

#if 0
RPThread::RPThread()
{
}

void RPThread::kill()
{
}

void RPThread::run()
{
}

String RPThread::readAllStdOut()
{
}

String RPThread::readAllStdErr()
{
}

bool RPThread::start(const Path &, const List<String> &args)
{
}

String RPThread::errorString() const
{
}

pid_t RPThread::pid() const
{
}

int RPThread::returnCode() const
{
}

bool RPThread::start(const std::shared_ptr<IndexerJob> &job)
{
}
#endif
