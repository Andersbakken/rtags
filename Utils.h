#ifndef utils_h
#define utils_h

#include <QtNetwork>
#include <QtCore>

const char *kindToString(int kind);
#ifdef EBUS_ENABLED
namespace EBus {
enum { SizeOfSize = sizeof(qint16) };
static inline int port()
{
    enum { DefaultPort = 6767 };
    QSettings settings(QSettings::IniFormat, QSettings::UserScope,
                       QCoreApplication::organizationName(), QCoreApplication::applicationName());
    return settings.value("daemonPort", DefaultPort).toInt();
}
template <typename T>
static inline bool writeToSocket(QAbstractSocket *dev, const T &t)
{
    // format sizeof(qint16) header with number of bytes in the rest of the package,
    // after that a QDataStream'ed T
    QByteArray byteArray;
    {
        QDataStream ds(&byteArray, QIODevice::WriteOnly);
        ds << t;
    }
    const qint16 size = byteArray.size();
    return (dev->write(reinterpret_cast<const char*>(&size), SizeOfSize) == SizeOfSize
            && dev->write(byteArray) == size);
}

enum ReadState {
    Error,
    Finished,
    WaitForData
};
template <typename T>
static inline ReadState readFromSocket(QAbstractSocket *dev, T &t, qint16 &size)
{
    switch (dev->state()) {
    case QAbstractSocket::ConnectingState:
    case QAbstractSocket::HostLookupState:
        return WaitForData;
    case QAbstractSocket::ConnectedState:
        break;
    default:
        return Error;
    }
    int available = dev->bytesAvailable();
    if (size == -1) {
        if (available < SizeOfSize)
            return WaitForData;
        if (dev->read(reinterpret_cast<char*>(&size), SizeOfSize) != SizeOfSize) {
            printf("%s %d: if (dev->read(reinterpret_cast<char*>(&size), SizeOfSize) != SizeOfSize) {\n", __FILE__, __LINE__);
            return Error;
        }
        available -= SizeOfSize;
    }
    if (available < size)
        return WaitForData;
    const QByteArray ba = dev->read(size);
    if (ba.size() != size) {
        printf("%s %d: if (ba.size() != size) {\n", __FILE__, __LINE__);
        return Error;
    }
    QDataStream ds(ba);
    ds >> t;
    return Finished;
}
}
#endif
class Options {
public:
    static bool s_verbose;
};
#include <QElapsedTimer>
#define DEBUG_FUNCTION_CALLS
#ifdef DEBUG_FUNCTION_CALLS // make this match a regexp in environment or something
class Timer : public QElapsedTimer
{
public:
    Timer(const char *func, const QString &args = QString())
        : m_func(func), m_args(args)
    {
        QMutexLocker lock(&s_mutex);
        if (Options::s_verbose) {
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
        if (Options::s_verbose) {
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
