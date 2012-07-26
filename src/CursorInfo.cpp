#include "CursorInfo.h"
#include "RTags.h"

ByteArray CursorInfo::toString() const
{
    ByteArray ret(16384, '\0');
    char *buf = ret.data();
    int pos = snprintf(buf, ret.size(), "CursorInfo(symbolLength: %u symbolName: %s kind: %s%s",
                       symbolLength, symbolName.constData(), RTags::eatString(clang_getCursorKindSpelling(kind)).constData(),
                       isDefinition ? " definition" : "");
    buf += pos;
    if (pos < ret.size() && target.isValid()) {
        const int w = snprintf(buf, ret.size() - pos, " target: %s", target.key().constData());
        buf += w;
        pos += w;
    }

    if (pos < ret.size() && !references.isEmpty()) {
        int w = snprintf(buf, ret.size() - pos, " references:\n");
        pos += w;
        buf += w;
        for (Set<Location>::const_iterator rit = references.begin(); rit != references.end() && w < ret.size(); ++rit) {
            const Location &l = *rit;
            w = snprintf(buf, ret.size() - pos, "    %s", l.key().constData());
            buf += w;
            pos += w;
        }
    }
    ret.truncate(pos);
    return ret;
}
