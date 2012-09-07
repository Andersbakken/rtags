#include "Location.h"
#include "Server.h"
#include "RTags.h"
Map<Path, uint32_t> Location::sPathsToIds;
Map<uint32_t, Path> Location::sIdsToPaths;
uint32_t Location::sLastId = 0;
ReadWriteLock Location::sLock;

ByteArray Location::key(unsigned flags) const
{
    if (isNull())
        return ByteArray();
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
    ByteArray ctx;
    if (flags & Location::ShowContext) {
        ctx += '\t';
        ctx += context();
        extra += ctx.size();
    }

    const Path p = path();

    ByteArray ret(p.size() + extra, '0');

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

ByteArray Location::context() const
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
                return ByteArray();
            }
        }
        char buf[1024] = { '\0' };
        const int len = RTags::readLine(f, buf, 1023);
        fclose(f);
        return ByteArray(buf, len);
    }
    if (f)
        fclose(f);
    return ByteArray();
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
        const int lineLen = RTags::readLine(f);
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
