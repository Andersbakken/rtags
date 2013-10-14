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

#ifndef SourceInformation_h
#define SourceInformation_h

#include <rct/List.h>
#include <rct/String.h>
#include <rct/Path.h>
#include "GccArguments.h"

class SourceInformation
{
public:
    SourceInformation()
        : fileId(0), language(GccArguments::NoLanguage), parsed(0)
    {}

    uint32_t fileId;
    Path compiler;
    GccArguments::Language language;
    List<String> args;
    time_t parsed;

    inline bool isJS() const
    {
        return args.isEmpty() && compiler.isEmpty() && sourceFile().endsWith(".js");
    }

    Path sourceFile() const { return Location::path(fileId); }

    inline bool isNull() const
    {
        return !fileId;
    }

    inline String toString() const
    {
        const Path src = sourceFile();
        String ret = src;
        if (parsed)
            ret += " Parsed: " + String::formatTime(parsed, String::DateTime);
        if (!isJS()) {
            if (parsed)
                ret += ' ';
            ret += (compiler + " " + String::join(args, ' '));
        }
        ret += ' ' + src;
        return ret;
    }

    void clear()
    {
        fileId = 0;
        parsed = 0;
        args.clear();
        compiler.clear();
    }
};

static inline Log operator<<(Log dbg, const SourceInformation &s)
{
    dbg << String::format<256>("SourceInformation(%s)", s.toString().constData());
    return dbg;
}

template <> inline Serializer &operator<<(Serializer &s, const SourceInformation &t)
{
    s << t.fileId << static_cast<uint8_t>(t.language) << t.parsed << t.compiler << t.args;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, SourceInformation &t)
{
    t.clear();
    uint8_t lang;
    s >> t.fileId >> lang >> t.parsed >> t.compiler >> t.args;
    t.language = static_cast<GccArguments::Language>(lang);
    return s;
}

#endif
