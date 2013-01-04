#ifndef ByteArray_h
#define ByteArray_h

#include <string>
#include <stdint.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "List.h"

class ByteArray
{
public:
    enum CaseSensitivity
    {
        CaseSensitive,
        CaseInsensitive
    };
    ByteArray(const char *data = 0, int len = -1)
    {
        if (data) {
            if (len == -1)
                len = strlen(data);
            mString = std::string(data, len);
        }
    }
    ByteArray(int len, char fillChar)
        : mString(len, fillChar)
    {}

    ByteArray(const ByteArray &ba)
        : mString(ba.mString)
    {}

    ByteArray(const std::string &str)
        : mString(str)
    {}

    ByteArray &operator=(const ByteArray &other)
    {
        mString = other.mString;
        return *this;
    }

    void assign(const char *data, int len = -1)
    {
        if (data) {
            if (len == -1)
                len = strlen(data);
            mString.assign(data, len);
        } else {
            clear();
        }
    }

    int lastIndexOf(char ch, int from = -1, CaseSensitivity cs = CaseSensitive) const
    {
        if (cs == CaseSensitive)
            return mString.rfind(ch, from == -1 ? std::string::npos : size_t(from));
        const char *data = mString.c_str();
        if (from == -1)
            from = mString.size() - 1;
        ch = tolower(ch);
        while (from >= 0) {
            if (tolower(data[from]) == ch)
                return from;
            --from;
        }
        return -1;
    }

    int indexOf(char ch, int from = 0, CaseSensitivity cs = CaseSensitive) const
    {
        if (cs == CaseSensitive)
            return mString.find(ch, from);
        const char *data = mString.c_str();
        ch = tolower(ch);
        const int size = mString.size();
        while (from < size) {
            if (tolower(data[from]) == ch)
                return from;
            ++from;
        }
        return -1;
    }

    bool contains(const ByteArray &other, CaseSensitivity cs = CaseSensitive) const
    {
        return indexOf(other, 0, cs) != -1;
    }

    bool contains(char ch, CaseSensitivity cs = CaseSensitive) const
    {
        return indexOf(ch, 0, cs) != -1;
    }

    int lastIndexOf(const ByteArray &ba, int from = -1, CaseSensitivity cs = CaseSensitive) const
    {
        if (ba.isEmpty())
            return -1;
        if (ba.size() == 1)
            return lastIndexOf(ba.first(), from, cs);
        if (cs == CaseSensitive)
            return mString.rfind(ba.mString, from == -1 ? std::string::npos : size_t(from));
        if (from == -1)
            from = mString.size() - 1;
        const ByteArray lowered = ba.toLower();
        const int needleSize = lowered.size();
        int matched = 0;
        while (from >= 0) {
            if (lowered.at(needleSize - matched - 1) != tolower(at(from))) {
                matched = 0;
            } else if (++matched == needleSize) {
                return from;
            }

            --from;
        }
        return -1;
    }

    int indexOf(const ByteArray &ba, int from = 0, CaseSensitivity cs = CaseSensitive) const
    {
        if (ba.isEmpty())
            return -1;
        if (ba.size() == 1)
            return indexOf(ba.first(), from, cs);
        if (cs == CaseSensitive)
            return mString.find(ba.mString, from);

        const ByteArray lowered = ba.toLower();
        const int count = size();
        int matched = 0;

        for (int i=from; i<count; ++i) {
            if (lowered.at(matched) != tolower(at(i))) {
                matched = 0;
            } else if (++matched == lowered.size()) {
                return i - matched + 1;
            }
        }
        return -1;
    }

    char first() const
    {
        return at(0);
    }

    char &first()
    {
        return operator[](0);
    }

    char last() const
    {
        assert(!isEmpty());
        return at(size() - 1);
    }

    char &last()
    {
        assert(!isEmpty());
        return operator[](size() - 1);
    }


    ByteArray toLower() const
    {
        std::string ret = mString;
        std::transform(ret.begin(), ret.end(), ret.begin(), ::tolower);
        return ret;
    }

    ByteArray toUpper() const
    {
        std::string ret = mString;
        std::transform(ret.begin(), ret.end(), ret.begin(), ::toupper);
        return ret;
    }

    char *data()
    {
        return &mString[0];
    }

    void clear()
    {
        mString.clear();
    }
    const char *data() const
    {
        return mString.data();
    }
    bool isEmpty() const
    {
        return mString.empty();
    }

    char at(int i) const
    {
        return mString.at(i);
    }

    char& operator[](int i)
    {
        return mString.operator[](i);
    }

    const char& operator[](int i) const
    {
        return mString.operator[](i);
    }

    const char *constData() const
    {
        return mString.data();
    }

    const char *nullTerminated() const
    {
        return mString.c_str();
    }

    int size() const
    {
        return mString.size();
    }

    void truncate(int size)
    {
        if (mString.size() > static_cast<size_t>(size))
            mString.resize(size);
    }

    void chop(int s)
    {
        mString.resize(size() - s);
    }

    void resize(int size)
    {
        mString.resize(size);
    }

    void reserve(int size)
    {
        mString.reserve(size);
    }

    void prepend(const ByteArray &other)
    {
        mString.insert(0, other);
    }

    void prepend(char ch)
    {
        mString.insert(0, &ch, 1);
    }

    void append(char ch)
    {
        mString += ch;
    }

    void append(const ByteArray &ba)
    {
        mString.append(ba);
    }

    void append(const char *str, int len)
    {
        mString.append(str, len);
    }

    void remove(int idx, int count)
    {
        mString.erase(idx, count);
    }

    ByteArray &operator+=(char ch)
    {
        mString += ch;
        return *this;
    }

    ByteArray &operator+=(const ByteArray &other)
    {
        mString += other.mString;
        return *this;
    }

    bool operator==(const ByteArray &other) const
    {
        return mString == other.mString;
    }

    bool operator!=(const ByteArray &other) const
    {
        return mString != other.mString;
    }

    bool operator<(const ByteArray &other) const
    {
        return mString < other.mString;
    }

    bool operator>(const ByteArray &other) const
    {
        return mString > other.mString;
    }

    bool endsWith(char ch) const
    {
        const int s = mString.size();
        return s && at(s - 1) == ch;
    }

    bool startsWith(char ch, CaseSensitivity c = CaseSensitive) const
    {
        if (!isEmpty()) {
            return (c == CaseInsensitive
                    ? tolower(at(0)) == tolower(ch)
                    : at(0) == ch);
        }
        return false;
    }

    bool endsWith(const ByteArray &str, CaseSensitivity cs = CaseSensitive) const
    {
        const int len = str.size();
        const int s = mString.size();
        if (s >= len) {
            return (cs == CaseInsensitive
                    ? !strncasecmp(str.constData(), constData() + s - len, len)
                    : !strncmp(str.constData(), constData() + s - len, len));
        }
        return false;
    }

    bool startsWith(const ByteArray &str, CaseSensitivity cs = CaseSensitive) const
    {
        const int s = mString.size();
        const int len = str.size();
        if (s >= len) {
            return (cs == CaseInsensitive
                    ? !strncasecmp(str.constData(), constData(), len)
                    : !strncmp(str.constData(), constData(), len));
        }
        return false;
    }

    void replace(int idx, int len, const ByteArray &with)
    {
        mString.replace(idx, len, with.mString);
    }

    void replace(const ByteArray &from, const ByteArray &to)
    {
        int idx = 0;
        while (true) {
            idx = indexOf(from, idx);
            if (idx == -1)
                break;
            replace(idx, from.size(), to);
            idx += to.size();
        }
    }

    int replace(char from, char to)
    {
        int count = 0;
        for (int i=size() - 1; i>=0; --i) {
            char &ch = operator[](i);
            if (ch == from) {
                ch = to;
                ++count;
            }
        }
        return count;
    }

    ByteArray mid(int from, int l = -1) const
    {
        if (l == -1)
            l = size() - from;
        if (from == 0 && l == size())
            return *this;
        return mString.substr(from, l);
    }

    ByteArray left(int l) const
    {
        return mString.substr(0, l);
    }

    ByteArray right(int l) const
    {
        return mString.substr(size() - l, l);
    }

    operator std::string() const
    {
        return mString;
    }

    List<ByteArray> split(char ch) const
    {
        List<ByteArray> ret;
        int last = 0;
        while (1) {
            const int next = indexOf(ch, last);
            if (next == -1)
                break;
            ret.append(mid(last, next - last));
            last = next + 1;
        }
        ret.append(mid(last));
        return ret;
    }

    unsigned long long toULongLong(bool *ok = 0) const
    {
        errno = 0;
        const unsigned ret = strtoull(constData(), 0, 10);
        if (ok)
            *ok = !errno;
        return ret;
    }
    unsigned long long toLongLong(bool *ok = 0) const
    {
        errno = 0;
        const unsigned ret = strtoll(constData(), 0, 10);
        if (ok)
            *ok = !errno;
        return ret;
    }

    static ByteArray number(long long num)
    {
        char buf[32];
        const int w = ::snprintf(buf, sizeof(buf), "%lld", num);
        return ByteArray(buf, w);
    }

    static ByteArray join(const List<ByteArray> &list, char ch)
    {
        return ByteArray::join(list, ByteArray(&ch, 1));
    }

    static ByteArray join(const List<ByteArray> &list, const ByteArray &sep)
    {
        ByteArray ret;
        const int sepSize = sep.size();
        int size = std::max(0, list.size() - 1) * sepSize;
        const int count = list.size();
        for (int i=0; i<count; ++i)
            size += list.at(i).size();
        ret.reserve(size);
        for (int i=0; i<count; ++i) {
            const ByteArray &b = list.at(i);
            ret.append(b);
            if (sepSize && i + 1 < list.size())
                ret.append(sep);
        }
        return ret;
    }
    template <int StaticBufSize>
    static ByteArray format(const char *format, ...)
    {
        va_list args;
        va_start(args, format);
        const ByteArray ret = ByteArray::format<StaticBufSize>(format, args);
        va_end(args);
        return ret;
    }

    template <int StaticBufSize>
    static ByteArray format(const char *format, va_list args)
    {
        va_list copy;
        va_copy(copy, args);

        char buffer[StaticBufSize];
        char *ch = buffer;
        int size = vsnprintf(buffer, StaticBufSize, format, args);
        if (size >= StaticBufSize) {
            ch = reinterpret_cast<char*>(malloc(size + 1));
            size = vsnprintf(ch, size+1, format, copy);
        }
        ch[size] = 0;
        va_end(copy);
        const ByteArray ret(ch, size);
        if (ch != buffer)
            free(ch);
        return ret;
    }
private:
    std::string mString;
};

inline const ByteArray operator+(const ByteArray &l, const char *r)
{
    ByteArray ret = l;
    ret += r;
    return ret;
}

inline const ByteArray operator+(const char *l, const ByteArray &r)
{
    ByteArray ret = l;
    ret += r;
    return ret;
}

inline const ByteArray operator+(const ByteArray &l, char ch)
{
    ByteArray ret = l;
    ret += ch;
    return ret;
}

inline const ByteArray operator+(const ByteArray &l, const ByteArray &r)
{
    ByteArray ret = l;
    ret += r;
    return ret;
}

#endif
