#ifndef CursorKey_h
#define CursorKey_h

#include <clang-c/Index.h>
#include <QByteArray>
#include "AtomicString.h"
#include "RTags.h"

class CursorKey
{
public:
    CursorKey()
        : kind(CXCursor_FirstInvalid), line(0), col(0), off(0), def(false)
    {}
    CursorKey(const CXCursor &cursor)
        : kind(clang_getCursorKind(cursor)), line(0), col(0), off(0), def(false)
    {
        if (!clang_isInvalid(kind)) {
            CXSourceLocation loc = clang_getCursorLocation(cursor);
            CXFile file;
            clang_getInstantiationLocation(loc, &file, &line, &col, &off);
            CXString str = clang_getFileName(file);
            fileName = Path::resolved(clang_getCString(str));
            clang_disposeString(str);
            str = clang_getCursorDisplayName(cursor);
            symbolName = clang_getCString(str);
            clang_disposeString(str);
            def = (kind == CXCursor_MacroDefinition || kind == CXCursor_LabelStmt || clang_isCursorDefinition(cursor));
            // if (!isValid())
            //     clear();
        }
    }

    void clear()
    {
        kind = CXCursor_FirstInvalid;
        line = col = off = 0;
        def = false;
        symbolName.clear();
        fileName.clear();
    }

    bool isValid() const
    {
        return !fileName.isEmpty() && !symbolName.isEmpty();
    }

    bool isNull() const
    {
        return !isValid();
    }

    bool isDefinition() const
    {
        return isValid() && def;
    }

    bool operator<(const CursorKey &other) const
    {
        if (!isValid())
            return true;
        if (!other.isValid())
            return false;
        int ret = fileName.strcmp(other.fileName);
        if (ret < 0)
            return true;
        if (ret > 0)
            return false;
        if (off < other.off)
            return true;
        if (off > other.off)
            return false;
        ret = symbolName.strcmp(other.symbolName);
        if (ret < 0)
            return true;
        if (ret > 0)
            return false;
        return kind < other.kind;
    }

    bool operator==(const CursorKey &other) const
    {
        if (isNull())
            return other.isNull();
        return (kind == other.kind
                && off == other.off
                && fileName == other.fileName
                && symbolName == other.symbolName);
    }
    bool operator!=(const CursorKey &other) const
    {
        return !operator==(other);
    }

    QByteArray locationKey() const
    {
        QByteArray key(fileName.toByteArray());
        key += ":" + QByteArray::number(off);
        return key;
    }

    QByteArray toString() const
    {
        QByteArray out;
        int ints[] = { line, col };
        int intsSize = 4; // two for :, two for the first digit
        for (int i=0; i<2; ++i) {
            int v = ints[i];
            while (v >= 10) {
                v /= 10;
                ++intsSize;
            }
        }

        out.resize(fileName.size() + intsSize);
        snprintf(out.data(), out.size() + 1, "%s:%d:%d", fileName.constData(), line, col);
        return out;
    }

    CXCursorKind kind;
    AtomicString fileName;
    AtomicString symbolName;
    unsigned line, col, off;
    bool def;
};

struct Cursor {
    CursorKey key;
    QVector<AtomicString> parentNames;
};

static inline bool operator==(const Cursor &left, const Cursor &right)
{
    return left.key == right.key && left.parentNames == right.parentNames;
}

static inline QDataStream &operator<<(QDataStream &ds, const CursorKey &key)
{
    Q_ASSERT(key.isValid());
    ds << qint32(key.kind) << key.fileName << key.symbolName
       << key.line << key.col << key.off << key.def;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, CursorKey &key)
{
    qint32 kind;
    ds >> kind >> key.fileName >> key.symbolName
       >> key.line >> key.col >> key.off >> key.def;
    key.kind = static_cast<CXCursorKind>(kind);
    Q_ASSERT(key.isValid());
    return ds;
}

static inline QDataStream &operator<<(QDataStream &ds, const Cursor &cursor)
{
    ds << cursor.key << cursor.parentNames;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, Cursor &cursor)
{
    ds >> cursor.key >> cursor.parentNames;
    return ds;
}


static inline QDebug operator<<(QDebug d, const CursorKey& key)
{
    d.nospace() << RTags::eatString(clang_getCursorKindSpelling(key.kind)).constData() << ", "
                << (key.symbolName.isEmpty() ? "(no symbol)" : key.symbolName.toByteArray().constData()) << ", "
                << key.fileName.toByteArray().constData() << ':' << key.line << ':' << key.col << (key.def ? " def" : " nodef");
    return d.space();
}

static inline uint qHashHelper(const CursorKey &key, const AtomicString *strings, int stringCount)
{
    uint h = 0;
    if (!key.isNull()) {
#define HASHCHAR(ch)                            \
        h = (h << 4) + ch;                      \
        h ^= (h & 0xf0000000) >> 23;            \
        h &= 0x0fffffff;                        \
        ++h;

        QByteArray name = key.fileName.toByteArray();
        const char *ch = name.constData();
        Q_ASSERT(ch);
        while (*ch) {
            HASHCHAR(*ch);
            ++ch;
        }
        name = key.symbolName.toByteArray();
        ch = name.constData();
        Q_ASSERT(ch);
        while (*ch) {
            HASHCHAR(*ch);
            ++ch;
        }
        const uint16_t uints[] = { key.kind, key.off };
        for (int i=0; i<2; ++i) {
            ch = reinterpret_cast<const char*>(&uints[i]);
            for (int j=0; j<2; ++j) {
                HASHCHAR(*ch);
                ++ch;
            }
        }
    }
    for (int i=0; i<stringCount; ++i) {
        const char *ch = strings[i].constData();
        while (*ch) {
            HASHCHAR(*ch);
            ++ch;
        }
    }
#undef HASHCHAR
    return h;
}

static inline uint qHash(const CursorKey &key)
{
    return qHashHelper(key, 0, 0);
}

static inline uint qHash(const Cursor &cursor)
{
    return qHashHelper(cursor.key, cursor.parentNames.constData(), cursor.parentNames.size());
}



#endif
