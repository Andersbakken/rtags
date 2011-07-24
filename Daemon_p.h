#ifndef Daemon_p_h
#define Daemon_p_h

#include <QElapsedTimer>
#define DEBUG_FUNCTION_CALLS
#ifdef DEBUG_FUNCTION_CALLS
class Timer : public QElapsedTimer
{
public:
    Timer(const char *func, const QString &args = QString())
        : mFunc(func), mArgs(args)
    {
        qDebug("%s(%s) called", func, qPrintable(mArgs));
        start();
    }
    ~Timer()
    {
        const int e = elapsed();
        qDebug("%s(%s) returns (%d ms)", mFunc, qPrintable(mArgs), e);
    }
private:
    const char *mFunc;
    const QString mArgs;
};
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
