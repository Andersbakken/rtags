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

#include "Preprocessor.h"
#include "Server.h"

#include "rct/Connection.h"
#include "rct/Log.h"
#include "rct/Process.h"
#include "RTags.h"

const Flags<Source::CommandLineFlag> SourceFlags = (Source::IncludeSourceFile
                                                    | Source::IncludeExtraCompiler
                                                    | Source::ExcludeDefaultArguments
                                                    | Source::ExcludeDefaultIncludePaths
                                                    | Source::ExcludeDefaultDefines
                                                    | Source::IncludeIncludePaths
                                                    | Source::IncludeDefines);

Preprocessor::Preprocessor(Mode mode, const Source &source, const std::shared_ptr<Connection> &connection)
    : mSource(source), mConnection(connection)
{
    Server::instance()->filterBlockedArguments(mSource);
    mArgs = mSource.toCommandLine(SourceFlags);
    if (mode == Preprocess) {
        mArgs.append("-E");
    } else {
        assert(mode == Asm);
        mArgs.append("-S");
        mArgs.append("-o");
        mArgs.append("-");
    }
    mProcess.reset(new Process);
    mProcess->finished().connect(std::bind(&Preprocessor::onProcessFinished, this));
}

Preprocessor::~Preprocessor()
{
    if (mProcess && mProcess->returnCode() == Process::ReturnUnset) {
        mProcess->kill();
    }
}

void Preprocessor::preprocess()
{
    mProcess->start(mSource.compiler(), mArgs);
}

void Preprocessor::onProcessFinished()
{
    mConnection->client()->setWriteMode(SocketClient::Synchronous);
    mConnection->write<256>("/* %s %s */", mSource.compiler().constData(), String::join(mArgs, ' ').constData());
    mConnection->write(mProcess->readAllStdOut());
    const String err = mProcess->readAllStdErr();
    if (!err.isEmpty()) {
        mConnection->write<1024>("/* %s */", err.constData());
    }
    mConnection->finish();
    EventLoop::deleteLater(this);
}

