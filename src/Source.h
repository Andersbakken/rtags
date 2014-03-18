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

    uint32_t fileId, compilerId, buildRootId;
    uint64_t includePathHash;
    enum Language {
        NoLanguage,
        JavaScript,
        C,
        CPlusPlus,
        CPlusPlus11,
        CHeader,
        CPlusPlusHeader,
        CPlusPlus11Header,
        ObjectiveC,
        ObjectiveCPlusPlus
    } language;

    uint64_t parsed, crc;

    enum Flag {
        NoFlag = 0x0,
        NoRtti = 0x1
    };
    uint32_t flags;

    struct Define {
        String define;
        String value;

        inline String toString(unsigned int flags = 0) const;
        inline bool operator==(const Define &other) const { return !compare(other); }
        inline bool operator!=(const Define &other) const { return compare(other) != 0; }
        inline bool operator<(const Define &other) const { return compare(other) < 0; }
        inline bool operator>(const Define &other) const { return compare(other) > 0; }
        inline int compare(const Source::Define &other) const
        {
            int cmp = define.compare(other.define);
            if (!cmp)
                cmp = value.compare(other.value);
            return cmp;
        }
    };

    Set<Define> defines;
    struct Include {
        enum Type {
            Type_None,
            Type_Include,
            Type_System
        };
        Include(Type t = Type_None, const Path &p = Path())
            : type(t), path(p)
        {}

        Type type;
        Path path;

        inline String toString() const
        {
            if (type == Type_None)
                return String();
            return String::format<128>("-%s%s", type == Type_System ? "isystem" : "I",
                                       path.constData());
        }

        inline int compare(const Source::Include &other) const
        {
            if (type == other.type) {
                return path.compare(other.path);
            }
            return type < other.type;
        }

        inline bool operator==(const Include &other) const { return !compare(other); }
        inline bool operator!=(const Include &other) const { return compare(other) != 0; }
        inline bool operator<(const Include &other) const { return compare(other) < 0; }
        inline bool operator>(const Include &other) const { return compare(other) > 0; }
    };
    List<Include> includePaths;
    List<String> arguments;
    int32_t sysRootIndex;

    bool isValid() const { return fileId; }
    bool isNull() const  { return !fileId; }

    uint64_t key() const { return key(fileId, buildRootId); }

    static inline uint64_t key(uint32_t fileId, uint32_t buildRootId)
    {
        uint64_t ret = fileId;
        ret <<= 32;
        ret |= buildRootId;
        return ret;
    }
    static inline void decodeKey(uint64_t key, uint32_t &fileId, uint32_t &buildRootId)
    {
        fileId = static_cast<uint32_t>(key >> 32);
        buildRootId = static_cast<uint32_t>(key);
    }

    int compare(const Source &other) const;
    bool compareArguments(const Source &other) const;
    bool operator==(const Source &other) const;
    bool operator!=(const Source &other) const;
    bool operator<(const Source &other) const;
    bool operator>(const Source &other) const;

    enum { None = 0x00 }; // shared enum

    enum CommandLineMode {
        IncludeCompiler = 0x01,
        IncludeSourceFile = 0x02,
        IncludeDefines = 0x04,
        IncludeIncludepaths = 0x08,
        QuoteDefines = 0x10,
        FilterBlacklist = 0x20
    };

    List<String> toCommandLine(unsigned int flags) const;
    inline bool isIndexable() const;
    Path sourceFile() const;
    Path buildRoot() const;
    Path compiler() const;
    void clear();
    String toString() const;
    Path sysRoot() const { return arguments.value(sysRootIndex, "/"); }

    enum ParseFlag {
        Escape = 0x1
    };
    static Source parse(const String &cmdLine, const Path &pwd,
                        unsigned int flags, Path *unresolvedInputLocation = 0);
};

inline Source::Source()
    : fileId(0), compilerId(0), buildRootId(0), includePathHash(0),
      language(NoLanguage), parsed(0), crc(0), sysRootIndex(-1)
{
}

inline bool Source::isIndexable() const
{
    switch (language) {
    case C:
    case CPlusPlus:
    case CPlusPlus11:
    case ObjectiveC:
    case ObjectiveCPlusPlus:
        return true;
    default:
        break;
    }
    return false;
}

inline int Source::compare(const Source &other) const
{
    if (fileId < other.fileId) {
        return -1;
    } else if (fileId > other.fileId) {
        return 1;
    }

    if (compilerId < other.compilerId) {
        return -1;
    } else if (compilerId > other.compilerId) {
        return 1;
    }

    if (int cmp = arguments.compare(other.arguments)) {
        return cmp;
    }

    if (int cmp = defines.compare(other.defines)) {
        return cmp;
    }

    if (int cmp = includePaths.compare(other.includePaths)) {
        return cmp;
    }

    if (sysRootIndex < other.sysRootIndex) {
        return -1;
    } else if (sysRootIndex > other.sysRootIndex) {
        return 1;
    }

    if (language < other.language) {
        return -1;
    } else if (language > other.language) {
        return 1;
    }

    return 0;
}

inline bool Source::operator==(const Source &other) const
{
    return !compare(other);
}

inline bool Source::operator!=(const Source &other) const
{
    return compare(other);
}

inline bool Source::operator<(const Source &other) const
{
    return compare(other) < 0;
}

inline bool Source::operator>(const Source &other) const
{
    return compare(other) > 0;
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

template <> inline Serializer &operator<<(Serializer &s, const Source::Include &d)
{
    s << static_cast<uint8_t>(d.type) << d.path;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Source::Include &d)
{
    uint8_t type;
    s >> type >> d.path;
    d.type = static_cast<Source::Include::Type>(type);
    return s;
}

template <> inline Serializer &operator<<(Serializer &s, const Source &b)
{
    s << b.fileId << b.compilerId << b.buildRootId << static_cast<uint8_t>(b.language)
      << b.parsed << b.crc << b.flags << b.defines << b.includePaths << b.arguments << b.sysRootIndex
      << b.includePathHash;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Source &b)
{
    b.clear();
    uint8_t language;
    s >> b.fileId >> b.compilerId >> b.buildRootId >> language >> b.parsed >> b.crc >> b.flags
      >> b.defines >> b.includePaths >> b.arguments >> b.sysRootIndex >> b.includePathHash;
    b.language = static_cast<Source::Language>(language);
    return s;
}

static inline Log operator<<(Log dbg, const Source &s)
{
    dbg << String::format<256>("Source(%s)", s.toString().constData());
    return dbg;
}

static inline Log operator<<(Log dbg, const Source::Define &def)
{
    dbg << def.toString();
    return dbg;
}

static inline Log operator<<(Log dbg, const Source::Include &inc)
{
    dbg << inc.toString();
    return dbg;
}

inline String Source::Define::toString(unsigned int flags) const
{
    String ret;
    ret.reserve(2 + define.size() + value.size() + 5);
    ret += "-D";
    ret += define;
    if (!value.isEmpty()) {
        ret += '=';
        if (flags & Source::QuoteDefines) {
            String out = value;
            out.replace("\\", "\\\\");
            out.replace("\"", "\\\"");
            ret += out;
        } else {
            ret += value;
        }
    }
    return ret;
}


#endif
