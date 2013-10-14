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

#ifndef Source_h
#define Source_h

#include <cstdint>
#include <rct/Path.h>
#include <rct/Serializer.h>
#include <rct/List.h>

struct Source
{
    inline Source();

    uint32_t fileId, compilerId;
    enum Language {
        NoLanguage,
        JavaScript,
        C,
        CPlusPlus,
        CPlusPlus11,
        CHeader,
        CPlusPlusHeader,
        CPlusPlus11Header
    } language;

    time_t parsed;

    struct Define {
        String define;
        String value;

        inline String toString() const;
        inline bool operator==(const Define &other) const;
    };

    List<Define> defines;
    List<Path> includePaths;
    List<String> arguments;

    bool isValid() const { return fileId; }
    bool isNull() const  { return !fileId; }

    enum CommandLineMode {
        None = 0x0,
        IncludeCompiler = 0x1,
        IncludeSourceFile = 0x2
    };

    List<String> toCommandLine(unsigned int mode) const;
    bool compare(const Source &other) const; // ignores parsed
    Path sourceFile() const;
    Path compiler() const;
    void clear();
    String toString() const;

    static Source parse(const String &cmdLine, const Path &pwd, Path *unresolvedInputLocation = 0);
};

inline Source::Source()
    : fileId(0), compilerId(0), language(NoLanguage), parsed(0)
{
}

template <> inline Serializer &operator<<(Serializer &s, const Source::Define &d)
{
    s << d.define << d.value;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Source::Define &d)
{
    s >> d.define >> d.value;
    return s;
}

template <> inline Serializer &operator<<(Serializer &s, const Source &b)
{
    s << b.fileId << b.compilerId << static_cast<uint8_t>(b.language) << b.parsed
      << b.defines << b.includePaths << b.arguments;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Source &b)
{
    b.clear();
    uint8_t language;
    s >> b.fileId >> b.compilerId >> language >> b.parsed
      >> b.defines >> b.includePaths >> b.arguments;
    b.language = static_cast<Source::Language>(language);
    return s;
}

static inline Log operator<<(Log dbg, const Source &s)
{
    dbg << String::format<256>("Source(%s)", s.toString().constData());
    return dbg;
}

inline bool Source::Define::operator==(const Source::Define &other) const
{
    return define == other.define && value == other.value;
}

inline String Source::Define::toString() const
{
    String ret;
    ret.reserve(2 + define.size() + value.size());
    ret += "-D";
    ret += define;
    ret += value;
    return ret;
}


#endif
