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
    enum Flag {
        None = 0x00,
        Warning = 0x01,
        Error = 0x02,
        Fixit = 0x04,
        Note = 0x08,
        Skipped = 0x10,
        Type_Mask = Warning|Error|Fixit|Note|Skipped,
        TemplateOnly = 0x20,
        DisplayCategory = 0x40
    };

    Diagnostic()
        : length(-1), sourceFileId(0)
    {
    }

    Flag type() const;

    String message;
    int length;
    uint32_t sourceFileId;

    Map<Location, int> ranges;
    Diagnostics children;
    Flags<Flag> flags;
    bool isNull() const { return type() == None; }
};

RCT_FLAGS(Diagnostic::Flag);

inline Diagnostic::Flag Diagnostic::type() const
{
    return (flags & Type_Mask).cast<Flag>();
}

template <> inline Serializer &operator<<(Serializer &s, const Diagnostic &d)
{
    // SBROOT
    String tmessage = Sandbox::encoded(d.message);
    s << d.flags.cast<uint8_t>() << tmessage << d.length << d.sourceFileId << d.ranges << d.children;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Diagnostic &d)
{
    uint8_t flags;
    String tmessage;
    s >> flags >> tmessage >> d.length >> d.sourceFileId >> d.ranges >> d.children;
    // SBROOT
    d.message = Sandbox::decoded(std::move(tmessage));
    d.flags = static_cast<Diagnostic::Flag>(flags);
    return s;
}

#endif
