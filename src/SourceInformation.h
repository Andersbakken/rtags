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
    SourceInformation(const Path &source, const List<ByteArray> &a, const Path &comp)
        : sourceFile(source), args(a), compiler(comp), parsed(0)
    {}

    Path sourceFile;
    List<ByteArray> args;
    Path compiler;
    time_t parsed;
    bool operator==(const SourceInformation &other) const
    {
        // We're intentionally not comparing parsed here
        return (sourceFile == other.sourceFile && args == other.args && compiler == other.compiler);
    }
    bool operator!=(const SourceInformation &other) const
    {
        // We're intentionally not comparing parsed here
        return (sourceFile != other.sourceFile || args != other.args || compiler != other.compiler);
    }
};

template <> inline Serializer &operator<<(Serializer &s, const SourceInformation &t)
{
    s << t.sourceFile << t.args << t.compiler << t.parsed;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, SourceInformation &t)
{
    s >> t.sourceFile >> t.args >> t.compiler >> t.parsed;
    return s;
}


#endif
