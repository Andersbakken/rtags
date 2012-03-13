#ifndef Log_h
#define Log_h

#include <QString>
#include <QDebug>
#include <QExplicitlySharedDataPointer>
#include <QSharedData>

void log(int level, const char *format, ...);
void log(const char *format, ...);
bool initLogging(int logLevel, const QByteArray &logFile);
int logLevel();

class Log
{
public:
    Log(int level = 0);
    Log(const Log &other);
    Log &operator=(const Log &other);
    template <typename T> Log &operator<<(const T &t)
    {
        if (mData) {
            *mData->dbg << t;
            return *this;
        }
        return *this;
    }
private:
    class Data : public QSharedData
    {
    public:
        Data(int lvl)
            : level(lvl)
        {
            dbg = new QDebug(&out);
        }
        ~Data()
        {
            delete dbg;
            log(level, "%s", qPrintable(out));
        }

        const int level;
        QDebug *dbg;
        QString out;
    };

    QExplicitlySharedDataPointer<Data> mData;
};

static inline Log log(int level = 0)
{
    return Log(level);
}

#endif
