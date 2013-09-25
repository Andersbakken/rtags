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

String Location::key(unsigned flags) const
{
    if (isNull())
        return String();
    int extra = 0;
    const int off = offset();
    int line = 0, col = 0;
    if (flags & Location::Padded) {
        extra = 7;
    } else if (flags & Location::ShowLineNumbers && convertOffset(line, col)) {
        extra = RTags::digits(line) + RTags::digits(col) + 3;
    } else {
        flags &= ~Location::ShowLineNumbers;
        extra = RTags::digits(off) + 1;
    }
    String ctx;
    if (flags & Location::ShowContext) {
        ctx += '\t';
        ctx += context();
        extra += ctx.size();
    }

    const Path p = path();

    String ret(p.size() + extra, '0');

    if (flags & Location::Padded) {
        snprintf(ret.data(), ret.size() + extra + 1, "%s,%06d%s", p.constData(),
                 off, ctx.constData());
    } else if (flags & Location::ShowLineNumbers) {
        snprintf(ret.data(), ret.size() + extra + 1, "%s:%d:%d:%s", p.constData(),
                 line, col, ctx.constData());
    } else {
        snprintf(ret.data(), ret.size() + extra + 1, "%s,%d%s", p.constData(),
                 off, ctx.constData());
    }
    return ret;
}

String Location::context(int *column) const
{
    const uint32_t off = offset();
    uint32_t o = off;
    Path p = path();
    FILE *f = fopen(p.constData(), "r");
    if (f && !fseek(f, off, SEEK_SET)) {
        while (o > 0) {
            const char ch = fgetc(f);
            if (ch == '\n' && o != off)
                break;
            if (fseek(f, --o, SEEK_SET) == -1) {
                fclose(f);
                return String();
            }
        }
        char buf[1024] = { '\0' };
        const int len = Rct::readLine(f, buf, 1023);
        fclose(f);
        if (column)
            *column = (off - o - 1);
        return String(buf, len);
    }
    if (f)
        fclose(f);
    return String();
}

bool Location::convertOffset(int &line, int &col) const
{
    const uint32_t off = offset();
    Path p = path();
    FILE *f = fopen(p.constData(), "r");
    if (!f) {
        line = col = -1;
        return false;
    }
    line = 1;
    int last = 0;
    uint32_t idx = 0;
    while (true) {
        const int lineLen = Rct::readLine(f);
        if (lineLen == -1) {
            col = line = -1;
            fclose(f);
            return false;
        }
        idx += lineLen + 1;
        // printf("lineStart %d offset %d last %d lineLen %d\n", idx, offset, last, lineLen);
        if (idx > off) {
            col = off - last + 1;
            break;
        }
        last = idx;
        ++line;
    }
    fclose(f);
    return true;
}
