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
    size_t includePathHash;
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

    uint64_t parsed;

    struct Define {
        String define;
        String value;

        enum Flag {
            None = 0x0,
            Quote = 0x1
        };
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
    List<Path> includePaths;
    List<String> arguments;
    int sysRootIndex;

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

    enum CommandLineMode {
        None = 0x0,
        IncludeCompiler = 0x1,
        IncludeSourceFile = 0x2
    };

    List<String> toCommandLine(unsigned int mode = IncludeCompiler|IncludeSourceFile) const;
    inline bool isIndexable() const;
    Path sourceFile() const;
    Path buildRoot() const;
    Path compiler() const;
    void clear();
    String toString() const;
    Path sysRoot() const { return arguments.value(sysRootIndex, "/"); }

    static Source parse(const String &cmdLine, const Path &pwd, Path *unresolvedInputLocation = 0);
};

inline Source::Source()
    : fileId(0), compilerId(0), buildRootId(0), includePathHash(0),
      language(NoLanguage), parsed(0), sysRootIndex(-1)
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

template <> inline Serializer &operator<<(Serializer &s, const Source &b)
{
    s << b.fileId << b.compilerId << b.buildRootId << static_cast<uint8_t>(b.language) << b.parsed
      << b.defines << b.includePaths << b.arguments << b.sysRootIndex
      << b.includePathHash;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Source &b)
{
    b.clear();
    uint8_t language;
    s >> b.fileId >> b.compilerId >> b.buildRootId >> language >> b.parsed
      >> b.defines >> b.includePaths >> b.arguments >> b.sysRootIndex
      >> b.includePathHash;
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

inline String Source::Define::toString(unsigned int flags) const
{
    String ret;
    ret.reserve(2 + define.size() + value.size() + 1);
    ret += "-D";
    ret += define;
    if (!value.isEmpty()) {
        ret += '=';
        if (flags & Quote) {
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
