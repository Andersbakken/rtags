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

#include <rct/String.h>
#include <rct/Path.h>
#include <rct/Log.h>
#include "JSParser.h"

int main(int argc, char **argv)
{
    if (argc < 2) {
        printf("[%s] %s:%d: if (argc < 2) {} [after]\n", __func__, __FILE__, __LINE__);
        return 1;
    }
    String c;
    Path file(argv[1]);
    file.resolve();
    if (argc > 2) {
        c = argv[2];
    } else {
        c = file.readAll();
    }

    JSParser parser;
    if (!parser.init()) {
        printf("[%s] %s:%d: if (!parser.init()) { [after]\n", __func__, __FILE__, __LINE__);
        return 1;
    }
    Hash<int, JSCursor> cursors;
    Hash<String, Set<int> > symbolNames;
    String errors;
    if (parser.parse(file, c, cursors, symbolNames, errors)) {
        for (Hash<int, JSCursor>::const_iterator it = cursors.begin(); it != cursors.end(); ++it)
            error() << String::format<64>("%s,%d", file.constData(), it->first) << it->second;
        for (Hash<String, Set<int> >::const_iterator it = symbolNames.begin(); it != symbolNames.end(); ++it) {
            error() << it->first;
            for (Set<int>::const_iterator sit = it->second.begin(); sit != it->second.end(); ++sit) {
                error() << String::format<64>("  %s,%d", file.constData(), *sit);
            }
        }
        error() << errors;
    }

    return 0;
}
