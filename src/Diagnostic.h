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

#include <rct/String.h>
#include <rct/Serializer.h>
#include "RTags.h"

struct Diagnostic
{
    enum Type { None, Warning, Error, Fixit, Skipped };

    Diagnostic(Type t = None, const String &m = String(), int l = -1)
        : type(t), message(m), length(l)
    {
    }

    Type type;
    String message;
    int length;
};

template <> inline Serializer &operator<<(Serializer &s, const Diagnostic &d)
{
    s << static_cast<uint8_t>(d.type) << d.message << d.length;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Diagnostic &d)
{
    uint8_t type;
    s >> type >> d.message >> d.length;
    d.type = static_cast<Diagnostic::Type>(type);
    return s;
}

#endif
