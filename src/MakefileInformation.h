#ifndef MakefileInformation_h
#define MakefileInformation_h

#include <stdint.h>
#include "List.h"
#include "ByteArray.h"

struct MakefileInformation
{
    MakefileInformation(const List<ByteArray> &args = List<ByteArray>(),
                        const List<ByteArray> &flags = List<ByteArray>())
        : makefileArgs(args), extraCompilerFlags(flags)
    {}
    List<ByteArray> makefileArgs;
    List<ByteArray> extraCompilerFlags;

    static inline MakefileInformation fromString(const ByteArray &string, bool *ok = 0);
    inline ByteArray toString() const;
};

inline MakefileInformation MakefileInformation::fromString(const ByteArray &string, bool *ok)
{
    MakefileInformation ret;
    if (ok)
        *ok = true;
    if (!string.isEmpty()) {
        const List<ByteArray> split = string.split('|');
        switch (split.size()) {
        case 0:
            assert(0);
            return ret;
        case 1:
            ret.makefileArgs = string.split(' ');
            break;
        case 2:
            ret.makefileArgs = split.first().split(' ');
            ret.extraCompilerFlags = split.last().split(' ');
            break;
        default:
            if (ok)
                *ok = false;
        }
    }
    return ret;
}

inline ByteArray MakefileInformation::toString() const
{
    ByteArray ret;
    if (!makefileArgs.isEmpty())
        ret = ByteArray::join(makefileArgs, ' ');
    if (!extraCompilerFlags.isEmpty()) {
        ret += '|';
        ret += ByteArray::join(extraCompilerFlags, ' ');
    }
    return ret;
}


#endif
