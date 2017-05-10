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

#ifndef Source_h
#define Source_h

#include <cstdint>

#include "Location.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Path.h"
#include "rct/Serializer.h"

struct SourceCache;
class SourceList;
struct Source
{
    inline Source();

    uint32_t fileId, compilerId, buildRootId, compileCommandsFileId;
    Path extraCompiler;
    uint64_t includePathHash;
    enum Language {
        NoLanguage,
        C,
        CPlusPlus,
        CPlusPlus11,
        CHeader,
        CPlusPlusHeader,
        CPlusPlus11Header,
        ObjectiveC,
        ObjectiveCPlusPlus
    } language;

    static const char *languageName(Language language);

    enum Flag {
        NoFlag = 0x0,
        NoRtti = 0x1,
        M32 = 0x2,
        M64 = 0x4
    };
    Flags<Flag> flags;

    enum CommandLineFlag {
        IncludeExtraCompiler = 0x001,
        IncludeCompiler = 0x002|IncludeExtraCompiler,
        IncludeSourceFile = 0x004,
        IncludeDefines = 0x008,
        IncludeIncludePaths = 0x010,
        QuoteDefines = 0x020,
        FilterBlacklist = 0x040,
        ExcludeDefaultArguments = 0x080,
        ExcludeDefaultIncludePaths = 0x100,
        ExcludeDefaultDefines = 0x200,
        IncludeRTagsConfig = 0x400,
        PCHEnabled = 0x800,
        IncludeOutputFilename = 0x1000,
        Default = IncludeDefines|IncludeIncludePaths|FilterBlacklist|IncludeRTagsConfig|IncludeOutputFilename,
    };

    struct Define {
        Define(const String &def = String(), const String &val = String())
            : define(def), value(val)
        {}
        String define;
        String value;

        inline String toString(Flags<CommandLineFlag> flags = Flags<CommandLineFlag>()) const;
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
            Type_Quote,
            Type_Framework,
            Type_System,
            Type_SystemFramework,
            Type_FileInclude
        };
        Include(Type t = Type_None, const Path &p = Path())
            : type(t), path(p)
        {}
        bool isPch() const;

        Type type;
        Path path;

        inline String toString() const
        {
            switch (type) {
            case Type_Include: return String::format<128>("-I%s", path.constData());
            case Type_Quote: return String::format<128>("-iquote %s", path.constData());
            case Type_Framework: return String::format<128>("-F%s", path.constData());
            case Type_System: return String::format<128>("-isystem %s", path.constData());
            case Type_SystemFramework: return String::format<128>("-iframework %s", path.constData());
            case Type_FileInclude: return String::format<128>("-include %s", path.constData());
            case Type_None: break;
            }
            return String();
        }

        inline int compare(const Source::Include &other) const
        {
            if (type != other.type) {
                return type < other.type ? -1 : 1;
            }
            return path.compare(other.path);
        }

        inline bool operator==(const Include &other) const { return !compare(other); }
        inline bool operator!=(const Include &other) const { return compare(other) != 0; }
        inline bool operator<(const Include &other) const { return compare(other) < 0; }
        inline bool operator>(const Include &other) const { return compare(other) > 0; }
    };
    List<Include> includePaths;
    List<String> arguments;
    int32_t sysRootIndex;
    Path directory;
    Path outputFilename;

    bool isValid() const { return fileId; }
    bool isNull() const  { return !fileId; }

    int compare(const Source &other) const;
    bool compareArguments(const Source &other) const;
    bool operator==(const Source &other) const;
    bool operator!=(const Source &other) const;
    bool operator<(const Source &other) const;
    bool operator>(const Source &other) const;

    List<String> toCommandLine(Flags<CommandLineFlag> flags = Flags<CommandLineFlag>(), bool *usedPch = 0) const;
    inline bool isIndexable() const;
    static inline bool isIndexable(Language lang);

    Path sourceFile() const;
    Path buildRoot() const;
    Path compileCommands() const;
    Path compiler() const;
    void clear();
    String toString() const;
    Path sysRoot() const { return arguments.value(sysRootIndex, "/"); }

    static SourceList parse(const String &cmdLine,
                              const Path &pwd,
                              const List<String> &environment,
                              List<Path> *unresolvedInputLocation = 0,
                              SourceCache *cache = 0);
    enum EncodeMode {
        IgnoreSandbox,
        EncodeSandbox
    };
    void encode(Serializer &serializer, EncodeMode mode) const;
    void decode(Deserializer &deserializer, EncodeMode mode);
};

RCT_FLAGS(Source::Flag);
RCT_FLAGS(Source::CommandLineFlag);

inline Source::Source()
    : fileId(0), compilerId(0), buildRootId(0), includePathHash(0),
      language(NoLanguage), sysRootIndex(-1)
{
}

inline const char *Source::languageName(Language language)
{
    switch (language) {
    case NoLanguage: return "NoLanguage";
    case C: return "C";
    case CPlusPlus: return "CPlusPlus";
    case CPlusPlus11: return "CPlusPlus11";
    case CHeader: return "CHeader";
    case CPlusPlusHeader: return "CPlusPlusHeader";
    case CPlusPlus11Header: return "CPlusPlus11Header";
    case ObjectiveC: return "ObjectiveC";
    case ObjectiveCPlusPlus: return "ObjectiveCPlusPlus";
    }
    return "";
}

inline bool Source::isIndexable() const
{
    return Source::isIndexable(language);
}

inline bool Source::isIndexable(Language lang)
{
    switch (lang) {
    case C:
    case CPlusPlus:
    case CPlusPlus11:
    case ObjectiveC:
    case ObjectiveCPlusPlus:
    case CPlusPlus11Header:
    case CPlusPlusHeader:
    case CHeader:
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
    b.encode(s, Source::EncodeSandbox);
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Source &b)
{
    b.decode(s, Source::EncodeSandbox);
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

inline String Source::Define::toString(Flags<CommandLineFlag> f) const
{
    String ret;
    ret.reserve(2 + define.size() + value.size() + 5);
    ret += "-D";
    ret += define;
    if (!value.isEmpty()) {
        ret += '=';
        if (f & Source::QuoteDefines) {
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

class SourceList : public List<Source>
{
public:
    uint64_t parsed = 0;

    uint32_t fileId() const { return isEmpty() ? 0 : front().fileId; }
};

template <>
inline Serializer &operator<<(Serializer &s, const SourceList &sources)
{
    s << static_cast<const List<Source> &>(sources) << sources.parsed;
    return s;
}

template <>
inline Deserializer &operator>>(Deserializer &d, SourceList &sources)
{
    d >> static_cast<List<Source> &>(sources) >> sources.parsed;
    return d;
}

#endif
