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
#include <rct/Connection.h>
#include <rct/Process.h>
#include <rct/Log.h>
#include "RTags.h"

Preprocessor::Preprocessor(const Source &source, const std::shared_ptr<Connection> &connection)
    : mSource(source), mConnection(connection)
{
    mProcess.reset(new Process);
    mProcess->finished().connect(std::bind(&Preprocessor::onProcessFinished, this));
}

const Flags<Source::CommandLineFlag> SourceFlags = (Source::IncludeSourceFile
                                                    | Source::ExcludeDefaultArguments
                                                    | Source::ExcludeDefaultIncludePaths
                                                    | Source::ExcludeDefaultDefines
                                                    | Source::IncludeIncludepaths
                                                    | Source::IncludeDefines);

void Preprocessor::preprocess()
{
    List<String> args = mSource.toCommandLine(SourceFlags);
    args.append("-E");

    mProcess->start(mSource.compiler(), args);
}

void Preprocessor::onProcessFinished()
{
    mConnection->client()->setWriteMode(SocketClient::Synchronous);
    const Flags<Source::CommandLineFlag> flags = (SourceFlags | Source::IncludeCompiler);
    mConnection->write<256>("// %s", String::join(mSource.toCommandLine(flags), ' ').constData());
    mConnection->write(mProcess->readAllStdOut());
    const String err = mProcess->readAllStdErr();
    if (!err.isEmpty()) {
        mConnection->write<1024>("/* %s */", err.constData());
    }
    mConnection->finish();
    EventLoop::deleteLater(this);
}
