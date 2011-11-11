#ifndef CursorKey_h
#define CursorKey_h

#include <clang-c/Index.h>
#include <QByteArray>
#include "AtomicString.h"
#include "RTags.h"

#ifndef CACHE_CURSORKEY
#define CACHE_CURSORKEY
#endif
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
            CXString fn = clang_getFileName(file);
            const char *fnStr = clang_getCString(fn);
            if (!fnStr || !strlen(fnStr) || !strncmp(fnStr, "/tmp/rtagspch_h", 15)) {
                clang_disposeString(fn);
                clear();
                return;
            }
            CXString sn = clang_getCursorDisplayName(cursor);
            const char *snStr = clang_getCString(sn);
            if (!snStr || !strlen(snStr)) {
                clang_disposeString(sn);
                clang_disposeString(fn);
                clear();
                return;
            }
            fileName = Path::resolved(fnStr);
            clang_disposeString(fn);
            QByteArray s = snStr;
            clang_disposeString(sn);
            if (kind == CXCursor_InclusionDirective) {
                const int last = s.lastIndexOf('/');
                if (last != -1) {
                    s.replace(0, last + 1, "include_");
                } else {
                    s.prepend("include_");
                }
            }
            symbolName = s;

            def = (kind == CXCursor_MacroDefinition
                   || kind == CXCursor_LabelStmt
                   || clang_isCursorDefinition(cursor));
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
#ifndef CACHE_CURSORKEY
        QByteArray mCachedLocationKey;
#else            
        if (mCachedLocationKey.isEmpty()) 
#endif
        {
            int intsSize = 2;
            int v = off;
            while (v >= 10) {
                v /= 10;
                ++intsSize;
            }

            mCachedLocationKey.resize(fileName.size() + intsSize);
            snprintf(mCachedLocationKey.data(), mCachedLocationKey.size() + 1, "%s:%d",
                     fileName.constData(), off);
        }
        return mCachedLocationKey;
    }

    QByteArray toString() const
    {
#ifndef CACHE_CURSORKEY
        QByteArray mCachedToString;
#else            
        if (mCachedToString.isEmpty()) 
#endif
        {
            int ints[] = { line, col };
            int intsSize = 5; // three for :, two for the first digit in line and col
            for (int i=0; i<2; ++i) {
                int v = ints[i];
                while (v >= 10) {
                    v /= 10;
                    ++intsSize;
                }
            }

            mCachedToString.resize(fileName.size() + intsSize);
            snprintf(mCachedToString.data(), mCachedToString.size() + 1, "%s:%d:%d:",
                     fileName.constData(), line, col);
        }
        return mCachedToString;
    }

    CXCursorKind kind;
    AtomicString fileName;
    AtomicString symbolName;
    unsigned line, col, off;
    bool def;

#ifdef CACHE_CURSORKEY
    mutable QByteArray mCachedToString, mCachedLocationKey;
#endif
};

struct Cursor {
    CursorKey key;
    QVector<AtomicString> parentNames;
    AtomicString containingFunction;
};

static inline bool operator==(const Cursor &left, const Cursor &right)
{
    return (left.key == right.key
            && left.containingFunction == right.containingFunction
            && left.parentNames == right.parentNames);
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
    ds << cursor.key << cursor.parentNames << cursor.containingFunction;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, Cursor &cursor)
{
    ds >> cursor.key >> cursor.parentNames >> cursor.containingFunction;
    return ds;
}


static inline QDebug operator<<(QDebug d, const CursorKey& key)
{
    d.nospace() << RTags::eatString(clang_getCursorKindSpelling(key.kind)).constData() << ", "
                << (key.symbolName.isEmpty() ? "(no symbol)" : key.symbolName.toByteArray().constData()) << ", "
                << key.fileName.toByteArray().constData() << ':' << key.line << ':' << key.col << ':' << (key.def ? " def" : " nodef");
    return d.space();
}

static inline QDebug operator<<(QDebug d, const Cursor& cursor)
{
    d << cursor.key << cursor.parentNames << cursor.containingFunction.toByteArray();
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
