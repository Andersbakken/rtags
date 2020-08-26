/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include "Preprocessor.h"

#include <assert.h>
#include <sys/types.h>
#include <functional>

#include "Server.h"
#include "rct/Connection.h"
#include "rct/Process.h"
#include "rct/EventLoop.h"
#include "rct/Flags.h"
#include "rct/Path.h"
#include "rct/SignalSlot.h"

const Flags<Source::CommandLineFlag> SourceFlags = (Source::IncludeSourceFile
                                                    | Source::IncludeExtraCompiler
                                                    | Source::ExcludeDefaultArguments
                                                    | Source::FilterBlacklist
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
    mConnection->write<256>("/* %s %s */", mSource.compiler().constData(), String::join(mArgs, ' ').constData());
    mConnection->write(mProcess->readAllStdOut());
    const String err = mProcess->readAllStdErr();
    if (!err.isEmpty()) {
        mConnection->write<1024>("/* %s */", err.constData());
    }
    mConnection->finish();
    EventLoop::deleteLater(this);
}
