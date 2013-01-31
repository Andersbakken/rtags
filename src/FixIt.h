#ifndef FixIt_h
#define FixIt_h

#include <stdint.h>
#include <rct/String.h>

struct FixIt
{
    inline FixIt(uint32_t s = 0, uint32_t e = 0, const String &t = String())
        : start(s), end(e), text(t)
    {
    }
    inline bool operator<(const FixIt &other) const
    {
        return start < other.start;
    }
    inline bool operator==(const FixIt &other) const
    {
        return (start == other.start && end == other.end && text == other.text);
    }

    uint32_t start, end;
    String text;
};

#endif
