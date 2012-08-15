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
#include <assert.h>

using namespace std::tr1;

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
void cleanupLogging();
int logLevel();
void restartTime();
class Log
{
public:
    Log(int level = 0);
    Log(const Log &other);
    Log &operator=(const Log &other);
    Log operator<<(long long number) { return addStringStream(number); }
    Log operator<<(unsigned long long number) { return addStringStream(number); }
    Log operator<<(long number) { return addStringStream(number); }
    Log operator<<(unsigned long number) { return addStringStream(number); }
    Log operator<<(int number) { return addStringStream(number); }
    Log operator<<(unsigned int number) { return addStringStream(number); }
    Log operator<<(short number) { return addStringStream(number); }
    Log operator<<(unsigned short number) { return addStringStream(number); }
    Log operator<<(char number) { return addStringStream(number); }
    Log operator<<(unsigned char number) { return addStringStream(number); }
    Log operator<<(float number) { return addStringStream(number); }
    Log operator<<(double number) { return addStringStream(number); }
    Log operator<<(long double number) { return addStringStream(number); }
    Log operator<<(bool boolean) { return write(boolean ? "true" : "false"); }
    Log operator<<(const char *string) { return write(string); }
    Log write(const char *data, int len = -1)
    {
        if (len == -1)
            len = strlen(data);
        assert(len >= 0);
        if (mData && len) {
            const int outLength = mData->out.size();
            if (mData->spacing && outLength && !isspace(mData->out.at(mData->out.size() - 1)) && !isspace(*data)) {
                mData->out.resize(outLength + len + 1);
                mData->out[outLength] = ' ';
                memcpy(mData->out.data() + outLength + 1, data, len);
            } else {
                mData->out.resize(outLength + len);
                memcpy(mData->out.data() + outLength, data, len);
            }
        }
        return *this;
    }
    bool setSpacing(bool on)
    {
        if (mData) {
            const bool ret = mData->spacing;
            mData->spacing = on;
            return ret;
        }
        return false;
    }

    bool spacing() const
    {
        return mData && mData->spacing;
    }
private:
    template <typename T> Log addStringStream(T t)
    {
        if (mData) {
            std::ostringstream str;
            str << t;
            const std::string string = str.str();
            return write(string.data(), string.size());
        }
        return *this;
    }
    class Data
    {
    public:
        Data(int lvl)
            : level(lvl), spacing(true)
        {
        }
        ~Data()
        {
            if (!out.isEmpty())
                log(level, "%s", out.nullTerminated());
        }

        const int level;
        ByteArray out;
        bool spacing;
    };

    shared_ptr<Data> mData;
};

class DisableSpacesScope
{
public:
    DisableSpacesScope(Log &log)
        : mLog(log)
    {
        mOld = mLog.setSpacing(false);
    }
    ~DisableSpacesScope()
    {
        mLog.setSpacing(mOld);
    }
    Log &mLog;
    bool mOld;
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
inline Log operator<<(Log stream, const List<T> &list)
{
    DisableSpacesScope scope(stream);
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
inline Log operator<<(Log stream, const Set<T> &set)
{
    DisableSpacesScope scope(stream);
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
inline Log operator<<(Log stream, const Map<Key, Value> &map)
{
    DisableSpacesScope scope(stream);
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

inline Log operator<<(Log stream, const ByteArray &byteArray)
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
