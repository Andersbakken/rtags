#ifndef FileInformation_h
#define FileInformation_h

#include "Serializer.h"
#include <stdint.h>
#include <List.h>
#include <ByteArray.h>

class FileInformation
{
public:
    FileInformation(time_t lt = 0, const List<ByteArray> &args = List<ByteArray>())
        : lastTouched(lt), compileArgs(args)
    {}

    time_t lastTouched;
    List<ByteArray> compileArgs;
};

static inline Serializer &operator<<(Serializer &s, const FileInformation &ci)
{
    s << ci.lastTouched << ci.compileArgs;
    return s;
}

static inline Deserializer &operator>>(Deserializer &ds, FileInformation &ci)
{
    ds >> ci.lastTouched >> ci.compileArgs;
    return ds;
}

#endif
