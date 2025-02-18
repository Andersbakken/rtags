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

#ifndef FixIt_h
#define FixIt_h

#include <stdint.h>

#include "rct/Serializer.h"
#include "rct/String.h"

struct FixIt
{
    inline FixIt(uint32_t l = 0, uint32_t c = 0, uint32_t len = 0, const String &t = String())
        : line(l), column(c), length(len), text(t)
    {
    }
    inline bool operator<(const FixIt &other) const
    {
        if (line < other.line)
            return true;
        if (line > other.line)
            return false;
        if (column < other.column)
            return true;
        if (column > other.column)
            return false;
        return length < other.length;
    }
    inline bool operator==(const FixIt &other) const
    {
        return (line == other.line && column == other.column && length == other.length && text == other.text);
    }

    uint32_t line, column, length;
    String text;
};

template <> inline Serializer &operator<<(Serializer &s, const FixIt &f)
{
    s << f.line << f.column << f.length << f.text;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, FixIt &f)
{
    s >> f.line >> f.column >> f.length >> f.text;
    return s;
}

#endif
