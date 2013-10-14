/* This file is part of RTags.

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

#include "Preprocessor.h"
#include <rct/Connection.h>
#include <rct/Process.h>
#include <rct/Log.h>
#include "RTags.h"

Preprocessor::Preprocessor(const Source &args, Connection *connection)
    : mArgs(args), mConnection(connection), mProc(0)
{
    mProc = new Process;
    mProc->finished().connect(std::bind(&Preprocessor::onProcessFinished, this));
}

Preprocessor::~Preprocessor()
{
    delete mProc;
}

void Preprocessor::preprocess()
{
    List<String> args = mArgs.toCommandLine(Source::IncludeSourceFile);
    args.append("-E");

    List<String> environ;
    environ << "PATH=/usr/local/bin:/usr/bin";
    // ### why this path?
    mProc->start(mArgs.compiler(), args, environ);
}

void Preprocessor::onProcessFinished()
{
    mConnection->client()->setWriteMode(SocketClient::Synchronous);
    mConnection->write<256>("// %s", String::join(mArgs.toCommandLine(Source::IncludeSourceFile|Source::IncludeCompiler), ' ').constData());
    mConnection->write(mProc->readAllStdOut());
    const String err = mProc->readAllStdErr();
    if (!err.isEmpty()) {
        mConnection->write<1024>("/* %s */", err.constData());
    }
    mConnection->finish();
    EventLoop::deleteLater(this);
}
