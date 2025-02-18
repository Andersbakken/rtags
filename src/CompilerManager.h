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

#ifndef CompilerManager_h
#define CompilerManager_h

#include "rct/List.h"
#include "rct/Path.h"
#include "rct/Serializer.h"
#include "rct/Flags.h"

struct Source;
class Path;

namespace CompilerManager
{
List<Path> compilers();
enum Flag {
    None = 0x0,
    IncludeDefines = 0x1,
    IncludeIncludePaths = 0x2
};
RCT_FLAGS(Flag);
void applyToSource(Source &source, Flags<Flag> flags);
}

#endif
