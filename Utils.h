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

// static inline bool resolvePath(QByteArray &fileName)
// {
//     char *resolved = realpath(fileName.constData(), 0);
//     if (resolved) {
//         fileName = resolved;
//         free(resolved);
//         return true;
//     }
//     return false;
// }

// static inline bool fileExists(const QByteArray &fileName)
// {

//     if (!fileName.isEmpty()) {
//         // ### symlinks?
//         struct stat st;
//         return !stat(fileName.constData(), &st) && S_ISREG(st.st_mode);
//     }
//     return false;
// }

class Options {
public:
    static bool s_verbose;
    static bool s_traceFunctionCalls;
};
#include <QElapsedTimer>
class Timer : public QElapsedTimer
{
public:
    Timer(const char *func, const QString &args = QString(),
          bool override = false)
        : m_func(func), m_args(args)
    {
        QMutexLocker lock(&s_mutex);
        if (Options::s_traceFunctionCalls || override) {
            qDebug("%s%s(%s) called (%s)",
                   indent().constData(), func, qPrintable(m_args),
                   qPrintable(QThread::currentThread()->objectName()));
        }
        s_indent += 2;
        start();
    }
    ~Timer()
    {
        QMutexLocker lock(&s_mutex);
        s_indent -= 2;
        const int e = elapsed();
        if (Options::s_traceFunctionCalls) {
            qDebug("%s%s(%s) returns (%d ms)",
                   indent().constData(), m_func, qPrintable(m_args), e);
        }
    }
    QByteArray indent() const
    {
        return QByteArray(s_indent, ' ');
    }
private:
    const char *m_func;
    const QString m_args;
    static int s_indent;
    static QMutex s_mutex;
};

#define DEBUG_FUNCTION_CALLS
#ifdef DEBUG_FUNCTION_CALLS // make this match a regexp in environment or something
static inline QDebug operator<<(QDebug dbg, const QFileInfo &fi)
{
    dbg << QString("QFileInfo(" + fi.absoluteFilePath() + ")");
    return dbg;
}
template <typename T>
void appendArg(QString &string, const T &t)
{
    if (!string.isEmpty())
        string += ' ';
    QDebug dbg(&string);
    dbg << t;
}
#define FUNC Timer noCollisions(__FUNCTION__);
#define FUNC1(a)                                                \
    QString noCollisionsString;                                 \
    appendArg(noCollisionsString, a);                           \
    Timer noCollisions(__FUNCTION__, noCollisionsString);
#define FUNC2(a, b)                                             \
    QString noCollisionsString;                                 \
    appendArg(noCollisionsString, a);                           \
    appendArg(noCollisionsString, b);                           \
    Timer noCollisions(__FUNCTION__, noCollisionsString);
#define FUNC3(a, b, c)                                          \
    QString noCollisionsString;                                 \
    appendArg(noCollisionsString, a);                           \
    appendArg(noCollisionsString, b);                           \
    appendArg(noCollisionsString, c);                           \
    Timer noCollisions(__FUNCTION__, noCollisionsString);
#define FUNC4(a, b, c, d)                                       \
    QString noCollisionsString;                                 \
    appendArg(noCollisionsString, a);                           \
    appendArg(noCollisionsString, b);                           \
    appendArg(noCollisionsString, c);                           \
    appendArg(noCollisionsString, d);                           \
    Timer noCollisions(__FUNCTION__, noCollisionsString);
#else
#define FUNC
#define FUNC1(a)
#define FUNC2(a, b)
#define FUNC3(a, b, c)
#define FUNC4(a, b, c, d)
#endif

#endif
