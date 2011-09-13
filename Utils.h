#ifndef Utils_h
#define Utils_h

#include <QtCore>
#include <clang-c/Index.h>

#ifdef EBUS_ENABLED
typedef QHash<QByteArray, QVariant> ByteArrayHash;
Q_DECLARE_METATYPE(ByteArrayHash)
Q_DECLARE_METATYPE(QList<QByteArray>)
#endif

const char *kindToString(int kind);
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
    return !clang_isInvalid(kind);
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

class Options {
public:
    static bool s_verbose;
    static bool s_traceFunctionCalls;
};
#endif
