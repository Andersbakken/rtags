/* This file is part of RTags (http://rtags.net).

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

#ifndef Diagnostic_h
#define Diagnostic_h

#include "rct/Serializer.h"
#include "rct/String.h"
#include "Location.h"
#include "Sandbox.h"

struct Diagnostic;
typedef Map<Location, Diagnostic> Diagnostics;

struct Diagnostic
{
    enum Type { None, Warning, Error, Fixit, Note, Skipped };

    Diagnostic()
        : type(None), length(-1)
    {
    }

    Type type;
    String message;
    int length;

    Map<Location, int> ranges;
    Diagnostics children;
    bool isNull() const { return type == None; }
};

template <> inline Serializer &operator<<(Serializer &s, const Diagnostic &d)
{
    // SBROOT
    String tmessage = Sandbox::encoded(d.message);
    s << static_cast<uint8_t>(d.type) << tmessage << d.length << d.ranges << d.children;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Diagnostic &d)
{
    uint8_t type;
    String tmessage;
    s >> type >> tmessage >> d.length >> d.ranges >> d.children;
    // SBROOT
    d.message = Sandbox::decoded(tmessage);
    d.type = static_cast<Diagnostic::Type>(type);
    return s;
}

#endif
