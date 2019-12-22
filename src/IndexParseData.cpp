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

#include "IndexParseData.h"
#include "Location.h"

bool IndexParseData::write(const std::function<bool(const String &)> &write, const Match &match) const
{
    auto process = [&write, &match](const String &str, const Sources &sss) {
        if (!sss.isEmpty()) {
            if (!write(str + ":"))
                return false;

            for (const auto &ss : sss) {
                const Path file = Location::path(ss.first);
                if (match.isEmpty() || match.match(file)) {
                    write("  " + file + ":");
                    for (const auto &s : ss.second) {
                        if (!write("    " + s.toString()))
                            return false;
                    }
                }
            }
        }
        return true;
    };
    if (!process("Sources", sources))
        return false;

    for (const auto &commands : compileCommands) {
        if (!process(Location::path(commands.first), commands.second.sources))
            return false;
    }
    return true;
}
