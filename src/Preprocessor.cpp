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
#include "RTagsClang.h"

Preprocessor::Preprocessor(const Source &args, Connection *connection)
    : mSource(args), mConnection(connection)
{
}

void Preprocessor::preprocess()
{
    const String preprocessed = RTags::preprocess(mSource);
    mConnection->client()->setWriteMode(SocketClient::Synchronous);
    mConnection->write(preprocessed);
    mConnection->write<256>("// %s", String::join(mSource.toCommandLine(Source::IncludeSourceFile|Source::IncludeCompiler), ' ').constData());
    mConnection->finish();
}
