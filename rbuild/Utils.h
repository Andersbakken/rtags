#ifndef Utils_h
#define Utils_h

#include <QtCore>
#include <clang-c/Index.h>

typedef QHash<QByteArray, QVariant> ByteArrayHash;
Q_DECLARE_METATYPE(ByteArrayHash)
Q_DECLARE_METATYPE(QList<QByteArray>)

const char *kindToString(CXCursorKind kind);
const char *completionChunkKindToString(int kind);
class Path;
bool locationFromString(const QByteArray &string, Path *path = 0, int *line = 0, int *column = 0);
static inline QByteArray eatString(CXString string)
{
    const QByteArray ret = clang_getCString(string);
    clang_disposeString(string);
    return ret;
}

static inline bool isValidCursor(CXCursor cursor)
{
    CXCursorKind kind = clang_getCursorKind(cursor);
    if (!clang_isInvalid(kind)) {
        CXSourceLocation loc = clang_getCursorLocation(cursor);
        CXFile file;
        unsigned int line, col, off;
        clang_getInstantiationLocation(loc, &file, &line, &col, &off);
        CXString filename = clang_getFileName(file);
        const char* str = clang_getCString(filename);
        if (!str || !strcmp(str, "")) {
            clang_disposeString(filename);
            return false;
        }
        clang_disposeString(filename);
        return true;
    }
    return false;
}


static inline void removeWhitespace(QByteArray &ba)
{
    int size = ba.size();
    int i = 0;
    while (i < size) {
        if (ba.at(i) == ' ') {
            ba.remove(i, 1);
            --size;
        } else {
            ++i;
        }
    }
}

QDebug operator<<(QDebug dbg, CXCursor cursor);

#endif
