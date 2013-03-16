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
    Map<int, JSCursor> cursors;
    Map<String, Set<int> > symbolNames;
    String errors;
    if (parser.parse(file, c, cursors, symbolNames, errors)) {
        for (Map<int, JSCursor>::const_iterator it = cursors.begin(); it != cursors.end(); ++it)
            error() << String::format<64>("%s,%d", file.constData(), it->first) << it->second;
        for (Map<String, Set<int> >::const_iterator it = symbolNames.begin(); it != symbolNames.end(); ++it) {
            error() << it->first;
            for (Set<int>::const_iterator sit = it->second.begin(); sit != it->second.end(); ++sit) {
                error() << String::format<64>("  %s,%d", file.constData(), *sit);
            }
        }
        error() << errors;
    }

    return 0;
}
