#ifndef SourceInformation_h
#define SourceInformation_h

#include <rct/List.h>
#include <rct/String.h>
#include <rct/Path.h>

class SourceInformation
{
public:
    SourceInformation()
        : parsed(0)
    {}

    Path sourceFile;
    Path compiler;
    List<String> args;

    inline bool isJS() const
    {
        return args.isEmpty() && compiler.isEmpty() && sourceFile.endsWith(".js");
    }

    time_t parsed;

    inline bool isNull() const
    {
        return sourceFile.isEmpty();
    }

    inline String toString() const
    {
        String ret = sourceFile;
        if (parsed)
            ret += " Parsed: " + String::formatTime(parsed, String::DateTime);
        if (!isJS())
            ret += (compiler + " " + String::join(args, ' '));
        return ret;
    }
};

template <> inline Serializer &operator<<(Serializer &s, const SourceInformation &t)
{
    s << t.sourceFile << t.parsed << t.compiler << t.args;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, SourceInformation &t)
{
    s >> t.sourceFile >> t.parsed >> t.compiler >> t.args;
    return s;
}

static inline Log operator<<(Log dbg, const SourceInformation &s)
{
    dbg << String::format<256>("SourceInformation(%s)", s.toString().constData());
    return dbg;
}

#endif
