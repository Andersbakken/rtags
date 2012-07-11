#ifndef Log_h
#define Log_h

#include <tr1/memory>
#include <ByteArray.h>
#include <Map.h>
#include <Set.h>
#include <List.h>
#include <sstream>
#include <Path.h>
#include <cxxabi.h>

class Path;

enum LogLevel {
    CompilationError = -1,
    Error = 0,
    Warning = 1,
    Debug = 2,
    VerboseDebug = 3
};

class LogOutput
{
public:
    LogOutput(int logLevel);
    virtual ~LogOutput();

    virtual bool testLog(int level) const
    {
        switch (level) {
        case CompilationError:
            return mLogLevel == CompilationError;
        default:
            return level <= mLogLevel;
        }
    }
    virtual void log(const char */*msg*/, int /*len*/) { }

    int logLevel() const { return mLogLevel; }

private:
    int mLogLevel;
};

#ifdef __GNUC__
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
void logDirect(int level, const ByteArray &out);

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
            mData->dbg << t;
        }
        return *this;
    }
    Log &operator<<(const std::string &string)
    {
        if (mData) {
            mData->dbg << string;
        }
        return *this;
    }
private:
    class Data
    {
    public:
        Data(int lvl)
            : level(lvl)
        {
        }
        ~Data()
        {
            log(level, "%s", dbg.str().c_str());
        }

        const int level;
        std::ostringstream dbg;
    };

    std::tr1::shared_ptr<Data> mData;
};

template <typename T> inline ByteArray typeName()
{
    const char *name = typeid(T).name();
    char *ret = abi::__cxa_demangle(name, 0, 0, 0);
    ByteArray ba;
    if (ret) {
        ba = ret;
        free(ret);
    }
    return ba;
}

template <typename T>
inline std::ostringstream &operator<<(std::ostringstream &stream, const List<T> &list)
{
    stream << "List<" << typeName<T>().constData() << ">(";
    bool first = true;
    for (typename List<T>::const_iterator it = list.begin(); it != list.end(); ++it) {
        if (!first) {
            stream << ", ";
        } else {
            first = false;
        }
        stream << *it;
    }
    stream << ")";
    return stream;
}

template <typename T>
inline std::ostringstream &operator<<(std::ostringstream &stream, const Set<T> &set)
{
    stream << "List<" << typeName<T>().constData() << ">(";
    bool first = true;
    for (typename Set<T>::const_iterator it = set.begin(); it != set.end(); ++it) {
        if (!first) {
            stream << ", ";
        } else {
            first = false;
        }
        stream << *it;
    }
    stream << ")";
    return stream;
}

template <typename Key, typename Value>
inline std::ostringstream &operator<<(std::ostringstream &stream, const Map<Key, Value> &map)
{
    stream << "Key<" << typeName<Key>().constData() << ", " << typeName<Value>().constData() << ">(";
    bool first = true;
    for (typename Map<Key, Value>::const_iterator it = map.begin(); it != map.end(); ++it) {
        if (!first) {
            stream << ", ";
        } else {
            first = false;
        }
        const Key &key = it->first;
        const Value &value = it->second;
        stream << key << ": ";
        stream << value; // ### we have some operator issue here
    }
    stream << ")";
    return stream;
}

inline std::ostringstream &operator<<(std::ostringstream &stream, const ByteArray &byteArray)
{
    stream.write(byteArray.constData(), byteArray.size());
    return stream;
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
