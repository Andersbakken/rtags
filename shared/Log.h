#ifndef Log_h
#define Log_h

#include <QString>
#include <QDebug>
#include <QExplicitlySharedDataPointer>
#include <QSharedData>
#include <QVariant>
class Path;

enum LogLevel {
    Error = 0,
    Warning = 1,
    Debug = 2,
    VerboseDebug = 3
};
void log(int level, const char *format, ...);
void debug(const char *format, ...);
void warning(const char *format, ...);
void error(const char *format, ...);
enum LogFlag {
    Append = 0x1,
    DontRotate = 0x2
};
bool testLog(int level);
bool initLogging(int logLevel, const Path &logFile, unsigned flags);
int logLevel();
QByteArray logFile();

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
    template <typename K, typename V> Log &operator<<(const QHash<K, V> &hash)
    {
        if (mData) {
            QByteArray out = "QHash<";
            {
                const K key;
                const QVariant variant = qVariantFromValue<K>(key);
                if (!mData->out.isEmpty())
                    out.prepend('\n');
                out += variant.typeName();
            }
            out += ", ";
            {
                const V value;
                QVariant variant = qVariantFromValue<V>(value);
                out += variant.typeName();
                if (out.endsWith('>'))
                    out += ' ';
                out += ">(";
            }
            *mData->dbg << out.constData();
            mData->dbg->nospace() << '\n';
            for (typename QHash<K, V>::const_iterator it = hash.begin(); it != hash.end(); ++it) {
                mData->dbg->nospace() << "  " << it.key() << ": " << it.value();
                mData->dbg->nospace() << '\n';
            }
            *mData->dbg << ")\n";
            mData->dbg->maybeSpace();
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
            log(level, "%s", qPrintable(out.trimmed()));
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
static inline Log error()
{
    return Log(Error);
}

static inline Log warning()
{
    return Log(Warning);
}

static inline Log debug()
{
    return Log(Debug);
}

static inline Log verboseDebug()
{
    return Log(VerboseDebug);
}

#endif
