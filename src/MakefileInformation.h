#ifndef MakefileInformation_h
#define MakefileInformation_h

#include "Serializer.h"
#include <stdint.h>
#include <List.h>
#include <ByteArray.h>

struct MakefileInformation {
    MakefileInformation(time_t lt = 0,
                        const List<ByteArray> &args = List<ByteArray>(),
                        const List<ByteArray> &flags = List<ByteArray>())
        : lastTouched(lt), makefileArgs(args), extraFlags(flags)
    {}
    time_t lastTouched;
    List<ByteArray> makefileArgs;
    List<ByteArray> extraFlags;
};

static inline Serializer &operator<<(Serializer &s, const MakefileInformation &mi)
{
    s << mi.lastTouched << mi.makefileArgs << mi.extraFlags;
    return s;
}

static inline Deserializer &operator>>(Deserializer &s, MakefileInformation &mi)
{
    s >> mi.lastTouched >> mi.makefileArgs >> mi.extraFlags;
    return s;
}

#endif
