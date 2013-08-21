#ifndef SourceInformation_h
#define SourceInformation_h

#include <rct/List.h>
#include <rct/String.h>
#include <rct/Path.h>

class SourceInformation
{
public:
    SourceInformation()
        : fileId(0), parsed(0)
    {}

    uint32_t fileId;
    Path compiler;
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
        String ret = sourceFile();
        if (parsed)
            ret += " Parsed: " + String::formatTime(parsed, String::DateTime);
        if (!isJS()) {
            if (parsed)
                ret += ' ';
            ret += (compiler + " " + String::join(args, ' '));
        }
        return ret;
    }
};

template <> inline Serializer &operator<<(Serializer &s, const SourceInformation &t)
{
    s << t.fileId << t.parsed << t.compiler << t.args;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, SourceInformation &t)
{
    s >> t.fileId >> t.parsed >> t.compiler >> t.args;
    return s;
}

static inline Log operator<<(Log dbg, const SourceInformation &s)
{
    dbg << String::format<256>("SourceInformation(%s)", s.toString().constData());
    return dbg;
}

#endif
