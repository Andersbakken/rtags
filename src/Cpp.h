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

#ifndef Cpp_h
#define Cpp_h

#include <Source.h>
#include <rct/String.h>
#include <rct/List.h>
#include <rct/Serializer.h>
#include "RTags.h"

struct Cpp
{
    String preprocessed;
    uint64_t time;

    struct Diagnostic {
        enum Type {
            Note,
            Error,
            Warning
        };
        Type type;
        String text;
        Location location;
    };

    List<Diagnostic> diagnostics;
    SymbolMap macroCursors;
    SymbolNameMap macroNames;
};

template <> inline Serializer &operator<<(Serializer &s, const Cpp::Diagnostic &d)
{
    s << static_cast<uint8_t>(d.type) << d.text << d.location;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Cpp::Diagnostic &d)
{
    uint8_t type;
    s >> type >> d.text >> d.location;
    d.type = static_cast<Cpp::Diagnostic::Type>(type);
    return s;
}

template <> inline Serializer &operator<<(Serializer &s, const Cpp &c)
{
    s << c.preprocessed << c.time << c.diagnostics << c.macroCursors << c.macroNames;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Cpp &c)
{
    s >> c.preprocessed >> c.time >> c.diagnostics >> c.macroCursors >> c.macroNames;
    return s;
}

#endif
