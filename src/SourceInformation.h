#ifndef SourceInformation_h
#define SourceInformation_h

#include "List.h"
#include "ByteArray.h"
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
        Build(const Path &c = Path(), const List<ByteArray> &a = List<ByteArray>())
            : compiler(c), args(a)
        {}
        Path compiler;
        List<ByteArray> args;
    };
    List<Build> builds;
    time_t parsed;

    bool isNull() const
    {
        return sourceFile.isEmpty();
    }

    bool merge(const Path &compiler, const List<ByteArray> &args)
    {
        for (int i=0; i<builds.size(); ++i) {
            if (builds.at(i).compiler == compiler && builds.at(i).args == args)
                return false;
        }
        builds.append(Build(compiler, args));
        return true;
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
    ByteArray out = ByteArray::format<64>("SourceInformation(%s (%s)", s.sourceFile.constData(),
                                          s.parsed ? ByteArray::formatTime(s.parsed, ByteArray::DateTime).constData() : "not parsed");
    for (int i=0; i<s.builds.size(); ++i) {
        out += ByteArray::format<256>(" %s %s", s.builds.at(i).compiler.constData(),
                                      ByteArray::join(s.builds.at(i).args, ' ').constData());
    }
    dbg << out;

    return dbg;
}

#endif
