#ifndef ByteArray_h
#define ByteArray_h

#include <string>
#include <stdint.h>
#include <errno.h>
#include <List.h>
#include <stdio.h>
#include <string.h>

class ByteArray
{
public:
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

    int lastIndexOf(char ch, int from = -1) const
    {
        return mString.rfind(ch, from == -1 ? std::string::npos : size_t(from));
    }

    int indexOf(char ch, int from = 0) const
    {
        return mString.find(ch, from);
    }

    bool contains(const ByteArray &other) const
    {
        return indexOf(other) != -1;
    }

    bool contains(char ch) const
    {
        return indexOf(ch) != -1;
    }

    int lastIndexOf(const ByteArray &ba, int from = -1) const
    {
        return mString.rfind(ba.mString, from == -1 ? std::string::npos : size_t(from));
    }

    int indexOf(const ByteArray &ba, int from = 0) const
    {
        return mString.find(ba.mString, from);
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

    bool startsWith(char ch) const
    {
        return !isEmpty() && at(0) == ch;
    }


    bool endsWith(const ByteArray &str) const
    {
        const int len = str.size();
        const int s = mString.size();
        return s >= len && !strncmp(str.constData(), constData() + s - len, len);
    }

    bool startsWith(const ByteArray &str) const
    {
        const int s = mString.size();
        const int len = str.size();
        return s >= len && !strncmp(str.constData(), constData(), len);
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

    ByteArray mid(int from, int l = -1) const
    {
        if (l == -1)
            l = size() - from;
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
        // ### is this right?
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

    unsigned long long toUInt(bool *ok = 0) const
    {
        errno = 0;
        const unsigned ret = strtoull(constData(), 0, 10);
        if (ok)
            *ok = !errno;
        return ret;
    }

    static ByteArray number(long long num)
    {
        char buf[32];
        const int w = snprintf(buf, sizeof(buf), "%lld", num);
        return ByteArray(buf, w + 1);
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
