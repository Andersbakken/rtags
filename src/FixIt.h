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

#ifndef FixIt_h
#define FixIt_h

#include <stdint.h>
#include <rct/String.h>

struct FixIt
{
    inline FixIt(uint32_t s = 0, uint32_t e = 0, const String &t = String())
        : start(s), end(e), text(t)
    {
    }
    inline bool operator<(const FixIt &other) const
    {
        return start < other.start;
    }
    inline bool operator==(const FixIt &other) const
    {
        return (start == other.start && end == other.end && text == other.text);
    }

    uint32_t start, end;
    String text;
};

#endif
