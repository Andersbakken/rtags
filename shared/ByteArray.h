#ifndef ByteArray_h
#define ByteArray_h

#include <string>
#include <QDebug>
#include <QByteArray>
#include <errno.h>

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
    ByteArray(int len, char fillChar = '\0')
        : mString(len, fillChar)
    {}

    ByteArray(const ByteArray &ba)
        : mString(ba.mString)
    {}

    ByteArray(const QByteArray &ba)
        : mString(ba.constData(), ba.size())
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

    const char *constData() const
    {
        return mString.data();
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

    operator QString() const
    {
        return QString::fromStdString(mString);
    }

    QList<ByteArray> split(char ch) const
    {
        // ### is this right?
        QList<ByteArray> ret;
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
private:
    std::string mString;
};

inline uint hashString(const char *p, int n)
{
    uint h = 0;

    while (n--) {
        h = (h << 4) + *p++;
        h ^= (h & 0xf0000000) >> 23;
        h &= 0x0fffffff;
    }
    return h;
}


inline uint qHash(const ByteArray &ba)
{
    return hashString(ba.constData(), ba.size());
}

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

inline QDebug operator<<(QDebug dbg, const ByteArray &ba)
{
    dbg << ba.constData();
    return dbg;
}

static inline QDataStream &operator<<(QDataStream &ds, const ByteArray &ba)
{
    ds.writeBytes(ba.constData(), ba.size());
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, ByteArray &ba)
{
    quint32 size;
    ds >> size;
    ba.resize(size);
    ds.readRawData(ba.data(), size);
    return ds;
}

inline std::size_t hash_value(const ByteArray &ba)
{
    return hashString(ba.constData(), ba.size());
}

#endif
