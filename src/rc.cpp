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

#include <stdio.h>

#include "RClient.h"
#include "CommandLineParser.h"
#include "rct/String.h"

int main(int argc, char** argv)
{
    RClient rc;
    const CommandLineParser::ParseStatus status = rc.parse(argc, argv);
    switch (status.status) {
    case CommandLineParser::Parse_Ok:
        break;
    case CommandLineParser::Parse_Error:
        fprintf(stderr, "%s\n", status.error.constData());
        break;
    case CommandLineParser::Parse_Exec:
        rc.exec();
        break;
    }
    return rc.exitCode();
}
