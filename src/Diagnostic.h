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
        // TemplateOnly = 0x20, not used, no lack of bits here
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

    bool operator==(const Diagnostic &other) const
    {
        return (message == other.message
                && length == other.length
                && sourceFileId == other.sourceFileId
                && ranges == other.ranges
                && children == other.children
                && flags == other.flags);
    }

    bool operator!=(const Diagnostic &other) const
    {
        return !operator==(other);
    }
};

RCT_FLAGS(Diagnostic::Flag);

inline Diagnostic::Flag Diagnostic::type() const
{
    return Flags<Flag>(flags & Type_Mask).cast<Flag>();
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

static inline Log operator<<(Log dbg, const Diagnostic &diagnostic)
{
    const char *type;
    switch (diagnostic.type()) {
    case Diagnostic::None: type = "none"; break;
    case Diagnostic::Warning: type = "warning"; break;
    case Diagnostic::Error: type = "error"; break;
    case Diagnostic::Fixit: type = "fixit"; break;
    case Diagnostic::Note: type = "note"; break;
    case Diagnostic::Skipped: type = "skipped"; break;
    default:
        assert(0 && "Impossible impossibility");
        break;
    }

    dbg << String::format<1024>("Diagnostic(type: %s message: \"%s\" length: %d sourceFile: \"%s\"\nranges: ",
                                type, diagnostic.message.constData(), diagnostic.length,
                                Location::path(diagnostic.sourceFileId).constData());
    for (const auto &range : diagnostic.ranges) {
        dbg << String::format<1024>("%d:%d: %d chars", range.first.line(), range.first.column(), range.second);
    }
    if (diagnostic.flags & Diagnostic::DisplayCategory)
        dbg << " DisplayCategory";
    dbg << ")";
    return dbg;
}


#endif
