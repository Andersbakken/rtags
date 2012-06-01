#include "Log.h"
#include "Path.h"
#include <stdio.h>
#include <QDateTime>
#include <QMutex>
#include <errno.h>
#include <QCoreApplication>
#include <QElapsedTimer>

static unsigned sFlags = 0;
static QElapsedTimer sStart;
static QSet<Output*> sOutputs;
static QMutex sOutputsMutex;
static int sLevel = 0;

class FileOutput : public Output
{
public:
    FileOutput(FILE *f)
        : Output(INT_MAX), file(f)
    {
    }
    ~FileOutput()
    {
        if (file)
            fclose(file);
    }

    virtual void log(const char *msg, int)
    {
        fprintf(file, "%s\n", msg);
        fflush(file);
    }
    FILE *file;
};

class StderrOutput : public Output
{
public:
    StderrOutput(int lvl)
        : Output(lvl)
    {}
    virtual void log(const char *msg, int)
    {
        fprintf(stderr, "%s\n", msg);
    }
};


void restartTime()
{
    sStart.restart();
}

static inline QByteArray prettyTimeSinceStarted()
{
    quint64 elapsed = sStart.elapsed();
    char buf[128];
    enum { MS = 1,
           Second = 1000,
           Minute = Second * 60,
           Hour = Minute * 60
    };
    const int ratios[] = { Hour, Minute, Second, MS };
    int values[] = { 0, 0, 0, 0 };
    for (int i=0; i<4; ++i) {
        values[i] = elapsed / ratios[i];
        elapsed %= ratios[i];
    }
    snprintf(buf, sizeof(buf), "%02d:%02d:%02d:%03d: ", values[0], values[1], values[2], values[3]);
    return buf;
}

static void log(int level, const char *format, va_list v)
{
    if (!testLog(level))
        return;
    enum { Size = 16384 };
    char buf[Size];
    const QByteArray now = (sFlags & AbsoluteTime
                            ? QDateTime::currentDateTime().toString("dd/MM/yy hh:mm:ss: ").toLocal8Bit()
                            : prettyTimeSinceStarted());
    char *msg = buf;
    int n = vsnprintf(msg + now.size(), Size - now.size(), format, v);
    if (n == -1)
        return;
    if (n >= Size) {
        msg = new char[n + 1 + now.size()];
        n = vsnprintf(msg + now.size(), n + 1, format, v);
    }
    memcpy(msg, now.constData(), now.size());

    QMutexLocker lock(&sOutputsMutex);
    foreach(Output *output, sOutputs) {
        if (output->testLog(level)) {
            output->log(msg);
        }
    }

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

void verboseDebug(const char *format, ...)
{
    va_list v;
    va_start(v, format);
    log(VerboseDebug, format, v);
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

static inline void removeOutputs()
{
    QMutexLocker lock(&sOutputsMutex);
    qDeleteAll(sOutputs);
    sOutputs.clear();
}

bool testLog(int level)
{
    QMutexLocker lock(&sOutputsMutex);
    foreach(const Output *output, sOutputs)
        if (output->testLog(level))
            return true;
    return false;
}

int logLevel()
{
    return sLevel;
}

bool initLogging(int level, const Path &file, unsigned flags)
{
    sStart.start();
    sFlags = flags;
    sLevel = level;
    new StderrOutput(level);
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
        FILE *f = fopen(file.constData(), flags & Append ? "a" : "w");
        if (!f)
            return false;
        new FileOutput(f);
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
Output::Output(int logLevel)
    : mLogLevel(logLevel)
{
    QMutexLocker lock(&sOutputsMutex);
    sOutputs.insert(this);
}
Output::~Output()
{
    QMutexLocker lock(&sOutputsMutex);
    sOutputs.remove(this);
}
