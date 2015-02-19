/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "Location.h"
#include "Server.h"
#include <rct/Rct.h>
#include "RTags.h"
Hash<Path, uint32_t> Location::sPathsToIds;
Hash<uint32_t, Path> Location::sIdsToPaths;
uint32_t Location::sLastId = 0;
std::mutex Location::sMutex;
static inline uint64_t createMask(int startBit, int bitCount)
{
    uint64_t mask = 0;
    for (int i=startBit; i<startBit + bitCount; ++i) {
        mask |= (static_cast<uint64_t>(1) << i);
    }
    return mask;
}

const uint64_t Location::FILEID_MASK = createMask(0, FileBits);
const uint64_t Location::LINE_MASK = createMask(FileBits, LineBits);
const uint64_t Location::COLUMN_MASK = createMask(FileBits + LineBits, ColumnBits);

String Location::key(unsigned int flags) const
{
    if (isNull())
        return String();
    const unsigned int l = line();
    const unsigned int c = column();
    int extra = RTags::digits(l) + RTags::digits(c) + 3;
    String ctx;
    if (flags & Location::ShowContext) {
        ctx += '\t';
        ctx += context(flags);
        extra += ctx.size();
    }

    const Path p = path();

    String ret(p.size() + extra, '0');

    snprintf(ret.data(), ret.size() + extra + 1, "%s:%d:%d:%s", p.constData(),
             l, c, ctx.constData());
    return ret;
}

String Location::context(unsigned int flags) const
{
    String ret;
    FILE *f = fopen(path().constData(), "r");
    if (f) {
        const unsigned int l = line();
        for (unsigned i=1; i<l; ++i) {
            Rct::readLine(f);
        }

        char buf[1024] = { '\0' };
        const int len = Rct::readLine(f, buf, 1023);
        fclose(f);
        ret.assign(buf, len);
        // error() << "foobar" << ret << bool(flags & NoColor);
        if (!(flags & NoColor)) {
            const int col = column() - 1;
            int end = col;
            if (ret.at(end) == '~')
                ++end;
            while (ret.size() > end && (isalnum(ret.at(end)) || ret.at(end) == '_'))
                ++end;
            static const char *color = "\x1b[32;1m"; // dark yellow
            static const char *resetColor = "\x1b[0;0m";
            // error() << "foobar"<< end << col << ret.size();
            ret.insert(end, resetColor);
            ret.insert(col, color);
            // printf("[%s]\n", ret.constData());
        }
    }
    return ret;
}
