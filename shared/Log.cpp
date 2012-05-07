#include "Log.h"
#include "Path.h"
#include <stdio.h>
#include <QDateTime>
#include <QMutex>
#include <errno.h>
#include <QCoreApplication>

static int sLevel = 0;
static unsigned sFlags = 0;
static FILE *sFile = 0;
static QByteArray sLogFile;
static QElapsedTimer sStart;

static inline QByteArray prettyTimeSinceStarted()
{
    return QTime(0, 0, 0, sStart.elapsed()).toString("hh:mm:ss.zzz").toLocal8Bit();
}

static void log(int level, const char *format, va_list v)
{
    if (level > sLevel && !sFile)
        return;
    enum { Size = 16384 };
    char buf[Size];
    char *msg = buf;
    int n = vsnprintf(buf, Size, format, v);
    if (n == -1) {
        return;
    }
    if (n >= Size) {
        msg = new char[n + 1];
        n = vsnprintf(msg, n + 1, format, v);
    }
    const QByteArray now = (sFlags & AbsoluteTime ? QDateTime::currentDateTime().toString("dd/MM/yy hh:mm:ss").toLocal8Bit()
                            : prettyTimeSinceStarted());
    static QMutex mutex;
    QMutexLocker lock(&mutex); // serialize
#if 0
    static const char *names[] = { "Error: ", "Warning: ", "Debug: ", "Verbose: " };
    const char *name = level < static_cast<int>(sizeof(names)) / 4 ? names[level] : "";
    if (level <= sLevel)
        fprintf(stderr, "%s: %s%s\n", now.constData(), name, msg);
    if (sFile) {
        fprintf(sFile, "%s: %s%s\n", now.constData(), name, msg);
        fflush(sFile);
    }
#else
    if (level <= sLevel)
        fprintf(stderr, "%s: %s\n", now.constData(), msg);
    if (sFile) {
        fprintf(sFile, "%s: %s\n", now.constData(), msg);
        fflush(sFile);
    }
#endif

    if (msg != buf)
        delete []msg;
}

void log(int level, const char *format, ...)
{
    va_list v;
    va_start(v, format);
    log(level, format, v);
    va_end(v);
}

void debug(const char *format, ...)
{
    va_list v;
    va_start(v, format);
    log(Debug, format, v);
    va_end(v);
}

void warning(const char *format, ...)
{
    va_list v;
    va_start(v, format);
    log(Warning, format, v);
    va_end(v);
}

void error(const char *format, ...)
{
    va_list v;
    va_start(v, format);
    log(Error, format, v);
    va_end(v);
}

int logLevel()
{
    return sLevel;
}

QByteArray logFile()
{
    return sLogFile;
}

static inline void removeLogFile()
{
    Q_ASSERT(sFile);
    fflush(sFile);
    fclose(sFile);
    sFile = 0;
}

bool testLog(int level)
{
    return level <= sLevel || sFile;
}

bool initLogging(int level, const Path &file, unsigned flags)
{
    sStart.start();
    sFlags = flags;
    sLevel = level;
    if (!file.isEmpty()) {
        if (!(flags & (Append|DontRotate)) && file.exists()) {
            int i = 0;
            forever {
                Path rotated = file + "." + QByteArray::number(++i);
                if (!rotated.exists()) {
                    if (rename(file.constData(), rotated.constData())) {
                        char buf[1025];
                        strerror_r(errno, buf, 1024);
                        error() << "Couldn't rotate log file" << file << "to" << rotated << buf;
                    }
                    break;
                }
            }
        }
        sFile = fopen(file.constData(), flags & Append ? "a" : "w");
        if (!sFile)
            return false;
        sLogFile = file;
        qAddPostRoutine(removeLogFile);
    }
    return true;
}

Log::Log(int level)
{
    if (testLog(level))
        mData = new Data(level);
}

Log::Log(const Log &other)
    : mData(other.mData)
{
}

Log &Log::operator=(const Log &other)
{
    mData = other.mData;
    return *this;
}
