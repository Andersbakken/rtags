#include "IniFile.h"
#include "RTags.h"

IniFile::IniFile(const Path &path)
    : mPath(path)
{
    mValid = syncFromFile();
}

enum Direction {
    Forwards = 1,
    Backwards = -1
};
static inline int eatSpaces(char *&ptr, Direction direction, int max)
{
    int count = 0;
    while (count < max && isspace(*ptr)) {
        ++count;
        ptr += direction;
    }
    return count;
}

bool IniFile::syncFromFile()
{
    mValues.clear();
    FILE *f = fopen(mPath.constData(), "r");
    if (!f)
        return false;

    ByteArray group;
    char buf[1024];
    int line = 0;
    while (true) {
        ++line;
        int len = RTags::readLine(f, buf, sizeof(buf) - 1);
        if (len == -1)
            break;

        char *hash = strchr(buf, '#');
        if (hash) {
            len = hash - buf;
        }
        char *start = buf;
        len -= eatSpaces(start, Forwards, len);
        char *end = start + len - 1; // off by one?
        len -= eatSpaces(end, Backwards, len);
        if (!len)
            continue;
        if (*start == '[') {
            if (*end != ']')  {
                mError = ByteArray::format<128>("Parse error at line %d (%s)", line, buf);
                return false;
            }
            group = ByteArray(start + 1, len - 2);
        } else if (group.isEmpty()) {
            mError = ByteArray::format<128>("Parse error at line %d (%s). Value without group", line, buf);
            return false;
        } else {
            if (hash)
                *hash = '\0';
            char *eq = strchr(start, '=');
            ByteArray key, value;
            if (eq) {
                *eq++ = '\0';
                value = eq;
            }
            key = start;
            mValues[group][key] = value;
        }
    }
    fclose(f);
    return true;
}
bool IniFile::syncToFile()
{
    FILE *f = fopen(mPath.constData(), "w");
    if (!f) {
        ::error("Can't open %s for writing", mPath.constData());
        return false;
    }

    for (MapMap::const_iterator it = mValues.begin(); it != mValues.end(); ++it) {
        const ByteArray &group = it->first;
        const Map<ByteArray, ByteArray> &values = it->second;
        fputc('[', f);
        fwrite(group.constData(), sizeof(char), group.size(), f);
        fwrite("]\n", sizeof(char), 2, f);
        for (Map<ByteArray, ByteArray>::const_iterator groupIt = values.begin(); groupIt != values.end(); ++groupIt) {
            fwrite(groupIt->first.constData(), sizeof(char), groupIt->first.size(), f);
            if (!groupIt->second.isEmpty())  {
                fputc('=', f);
                fwrite(groupIt->second.constData(), sizeof(char), groupIt->second.size(), f);
            }
            fputc('\n', f);
        }
    }

    fclose(f);
    return true;
}

bool IniFile::remove(const ByteArray &group, const ByteArray &key)
{
    MapMap::iterator it = mValues.find(group);
    if (it == mValues.end() || !it->second.remove(key))
        return false;
    if (it->second.isEmpty())
        mValues.erase(it);
    syncToFile();
    return true;
}

bool IniFile::removeGroup(const ByteArray &group)
{
    if (mValues.remove(group)) {
        syncToFile();
        return true;
    }
    return false;
}
bool IniFile::setValue(const ByteArray &group, const ByteArray &key, const ByteArray &value)
{
    if (group.contains('=')) {
        ::error("Invalid group key %s", group.constData());
        return false;
    }
    mValues[group][key] = value;
    return syncToFile();
}
