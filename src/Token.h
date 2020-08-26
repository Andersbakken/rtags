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

#ifndef Token_h
#define Token_h

#include <clang-c/Index.h>
#include <stdint.h>

#include "rct/Serializer.h"
#include "rct/Log.h"
#include "Location.h"
#include "rct/String.h"

struct Token
{
    CXTokenKind kind;
    String spelling;
    Location location;
    uint32_t offset, length;

    String toString() const;
};

template <> inline Serializer &operator<<(Serializer &s, const Token &t)
{
    s << static_cast<uint8_t>(t.kind) << t.spelling << t.location << t.offset << t.length;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Token &t)
{
    uint8_t kind;
    s >> kind >> t.spelling >> t.location >> t.offset >> t.length;
    t.kind = static_cast<CXTokenKind>(kind);
    return s;
}

static inline Log operator<<(Log dbg, const Token &token)
{
    const String out = "Token(" + token.toString() + ")";
    return (dbg << out);
}

#endif
