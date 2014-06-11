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

#ifndef Unit_h
#define Unit_h

#include "CursorInfo.h"
#include "RTags.h"
#include "Source.h"
#include <rct/List.h>
#include <rct/Serializer.h>
#include <rct/String.h>

struct Unit
{
    Source source;
    Path sourceFile;

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

    unsigned int flags;
    List<Diagnostic> diagnostics;
    SymbolMap macroCursors;
    SymbolNameMap macroNames;
    Map<Path, uint32_t> visited;
    int preprocessDuration;
};

template <> inline Serializer &operator<<(Serializer &s, const Unit::Diagnostic &d)
{
    s << static_cast<uint8_t>(d.type) << d.text << d.location;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Unit::Diagnostic &d)
{
    uint8_t type;
    s >> type >> d.text >> d.location;
    d.type = static_cast<Unit::Diagnostic::Type>(type);
    return s;
}

template <> inline Serializer &operator<<(Serializer &s, const Unit &c)
{
    s << c.source << c.sourceFile << c.preprocessed << c.time << c.flags << c.diagnostics;
    CursorInfo::serialize(s, c.macroCursors);
    s << c.macroNames << c.visited << c.preprocessDuration;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Unit &c)
{
    s >> c.source >> c.sourceFile >> c.preprocessed >> c.time >> c.flags >> c.diagnostics;
    CursorInfo::deserialize(s, c.macroCursors);
    s >> c.macroNames >> c.visited >> c.preprocessDuration;
    return s;
}

#endif
