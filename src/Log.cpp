#include "Log.h"
#include "Mutex.h"
#include "MutexLocker.h"
#include "Path.h"
#include <stdio.h>
#include <errno.h>
#include <Timer.h>

static unsigned sFlags = 0;
static Timer sStart;
static Set<Output*> sOutputs;
static Mutex sOutputsMutex;
static int sLevel = 0;

class FileOutput : public LogOutput
{
public:
    FileOutput(FILE *f)
        : LogOutput(INT_MAX), file(f)
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

class StdoutOutput : public LogOutput
{
public:
    StdoutOutput(int lvl)
        : LogOutput(lvl)
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

static inline ByteArray prettyTimeSinceStarted()
{
    uint64_t elapsed = sStart.elapsed();
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
    char *msg = buf;
    int n = vsnprintf(msg, Size, format, v);
    if (n == -1)
        return;
    if (n >= Size) {
        msg = new char[n + 1];
        n = vsnprintf(msg, n + 1, format, v);
    }

    MutexLocker lock(&sOutputsMutex);
    foreach(Output *output, sOutputs) {
        if (output->testLog(level)) {
            output->log(msg, n);
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
    MutexLocker lock(&sOutputsMutex);
    for (Set<Output*>::const_iterator it = sOutputs.begin(); it != sOutputs.end(); ++it)
        delete *it;
    sOutputs.clear();
}

bool testLog(int level)
{
    MutexLocker lock(&sOutputsMutex);
    for (Set<Output*>::const_iterator it = sOutputs.begin(); it != sOutputs.end(); ++it) {
        if ((*it)->testLog(level))
            return true;
    }
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
    new StdoutOutput(level);
    if (!file.isEmpty()) {
        if (!(flags & (Append|DontRotate)) && file.exists()) {
            int i = 0;
            forever {
                Path rotated = file + ".";
                rotated += ByteArray::number(++i);
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
        mData.reset(new Data(level));
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

Output::Output()
{
    MutexLocker lock(&sOutputsMutex);
    sOutputs.insert(this);
}

Output::~Output()
{
    MutexLocker lock(&sOutputsMutex);
    sOutputs.remove(this);
}

LogOutput::LogOutput(int logLevel)
    : mLogLevel(logLevel)
{
}

LogOutput::~LogOutput()
{
}
