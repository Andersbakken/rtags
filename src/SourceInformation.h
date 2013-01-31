#ifndef SourceInformation_h
#define SourceInformation_h

#include "List.h"
#include "String.h"
#include "Path.h"

class SourceInformation
{
public:
    SourceInformation()
        : parsed(0)
    {}

    Path sourceFile;
    struct Build
    {
        Build(const Path &c = Path(), const List<String> &a = List<String>())
            : compiler(c), args(a)
        {}
        Path compiler;
        List<String> args;
    };
    List<Build> builds;
    time_t parsed;

    bool isNull() const
    {
        return sourceFile.isEmpty();
    }

    bool merge(const Path &compiler, const List<String> &args)
    {
        for (int i=0; i<builds.size(); ++i) {
            if (builds.at(i).compiler == compiler && builds.at(i).args == args)
                return false;
        }
        builds.append(Build(compiler, args));
        return true;
    }
    inline String toString() const
    {
        String out = String::format<64>("%s %s\n", sourceFile.constData(),
                                              parsed ? ("Parsed: " +String::formatTime(parsed, String::DateTime)).constData() : "Not parsed");
        for (int i=0; i<builds.size(); ++i) {
            out += String::format<256>("  %s %s\n", builds.at(i).compiler.constData(),
                                          String::join(builds.at(i).args, ' ').constData());
        }
        return out;
    }
};

template <> inline Serializer &operator<<(Serializer &s, const SourceInformation &t)
{
    s << t.sourceFile << t.parsed << t.builds.size();
    for (int i=0; i<t.builds.size(); ++i) {
        s << t.builds.at(i).compiler << t.builds.at(i).args;
    }


    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, SourceInformation &t)
{
    s >> t.sourceFile >> t.parsed;
    int size;
    s >> size;
    t.builds.resize(size);
    for (int i=0; i<size; ++i) {
        s >> t.builds[i].compiler >> t.builds[i].args;
    }
    return s;
}

static inline Log operator<<(Log dbg, const SourceInformation &s)
{
    dbg << String::format<256>("SourceInformation(%s)", s.toString().constData());
    return dbg;
}

#endif
