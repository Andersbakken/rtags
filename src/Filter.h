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

#ifndef Filter_h
#define Filter_h

#include <fnmatch.h>

#include "rct/List.h"
#include "rct/Path.h"
#include "rct/String.h"

namespace Filter {
enum Result {
    Filtered,
    File,
    Source,
    Directory
};

static inline Result filter(const Path &path, const List<String> &filters = List<String>())
{
    const int size = filters.size();
    for (int i=0; i<size; ++i) {
        const String &filter = filters.at(i);
        if (!fnmatch(filter.constData(), path.constData(), 0) || path.contains(filter))
            return Filtered;
    }

    if (path.isDir())
        return Directory;

    const char *ext = path.extension();
    if (ext && (Path::isSource(ext) || Path::isHeader(ext)))
        return Source;
    return File;
}
}

#endif
