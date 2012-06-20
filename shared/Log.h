#ifndef Log_h
#define Log_h

#include <QString>
#include <QDebug>
#include <QExplicitlySharedDataPointer>
#include <QSharedData>
#include <QVariant>
#include <ByteArray.h>
#include <Hash.h>
#include <Set.h>
#include <List.h>

class Path;

enum LogLevel {
    Error = 0,
    Warning = 1,
    Debug = 2,
    VerboseDebug = 3
};

class Output
{
public:
    Output();
    virtual ~Output();

    virtual bool testLog(int level) const = 0;
    virtual void log(const char */*msg*/, int /*len*/) = 0;
};

class LogOutput : public Output
{
public:
    LogOutput(int logLevel);
    virtual ~LogOutput();

    virtual bool testLog(int level) const { return level <= mLogLevel; }
    virtual void log(const char */*msg*/, int /*len*/) { }

    int logLevel() const { return mLogLevel; }

private:
    int mLogLevel;
};

class EventOutput : public Output
{
public:
    EventOutput(int level);
    ~EventOutput();

    virtual bool testLog(int level) const { return mLevel & level; }
    virtual void log(const char */*msg*/, int /*len*/) { }

    int level() const { return mLevel; }

private:
    int mLevel;
};

#if defined(Q_CC_GNU)
void log(int level, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
void debug(const char *format, ...) __attribute__ ((format (printf, 1, 2)));
void verboseDebug(const char *format, ...) __attribute__ ((format (printf, 1, 2)));
void warning(const char *format, ...) __attribute__ ((format (printf, 1, 2)));
void error(const char *format, ...) __attribute__ ((format (printf, 1, 2)));
#else
void log(int level, const char *format, ...);
void debug(const char *format, ...);
void verboseDebug(const char *format, ...);
void warning(const char *format, ...);
void error(const char *format, ...);
#endif
enum LogFlag {
    Append = 0x1,
    DontRotate = 0x2,
    AbsoluteTime = 0x4
};
bool testLog(int level);
bool initLogging(int logLevel, const Path &logFile, unsigned flags);
int logLevel();
void restartTime();
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

    template <typename T> Log &operator<<(const List<T> &vector)
    {
        if (mData) {
            ByteArray out;
            if (mData->out.isEmpty())
                out += '\n';
            out += "std::vector<";
            {
                T key;
                const QVariant variant = qVariantFromValue<T>(key);
                out += variant.typeName();
                out += ">(";
            }
            *mData->dbg << out.constData();
            bool first = true;
            for (typename std::vector<T>::const_iterator it = vector.begin(); it != vector.end(); ++it) {
                if (!first) {
                    mData->dbg->nospace() << ", ";
                } else {
                    first = false;
                }
                mData->dbg->nospace() << *it;
            }
            *mData->dbg << ")";
            mData->dbg->maybeSpace();
            return *this;
        }
        return *this;
    }


    template <typename K, typename V> Log &operator<<(const Hash<K, V> &hash)
    {
        if (mData) {
            ByteArray out = "Hash<";
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
            for (typename Hash<K, V>::const_iterator it = hash.begin(); it != hash.end(); ++it) {
                mData->dbg->nospace() << "  " << it.key() << ": " << it.value();
                mData->dbg->nospace() << '\n';
            }
            *mData->dbg << ")\n";
            mData->dbg->maybeSpace();
            return *this;
        }
        return *this;
    }

    template <typename K> Log &operator<<(const Set<K> &set)
    {
        if (mData) {
            ByteArray out;
            if (mData->out.isEmpty())
                out += '\n';
            out += "Set<";
            {
                K key;
                const QVariant variant = qVariantFromValue<K>(key);
                out += variant.typeName();
                out += ">(";
            }
            *mData->dbg << out.constData();
            bool first = true;
            for (typename Set<K>::const_iterator it = set.begin(); it != set.end(); ++it) {
                if (!first) {
                    mData->dbg->nospace() << ", ";
                } else {
                    first = false;
                }
                mData->dbg->nospace() << *it;
            }
            *mData->dbg << ")";
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
