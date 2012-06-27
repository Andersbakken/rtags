#ifndef RTags_h
#define RTags_h

#include <typeinfo>
#include <ByteArray.h>
#include <Path.h>
#include <Log.h>
#include <memory>
#include <stdio.h>
#include <assert.h>
#include <getopt.h>

namespace RTags {
enum UnitType { CompileC, CompileCPlusPlus, PchC, PchCPlusPlus };

static inline Path rtagsDir() {
    char buf[128];
    int w = snprintf(buf, sizeof(buf), "%s/.rtags/", getenv("HOME"));
    return Path(buf, w);
}

enum KeyFlag {
    NoFlag = 0x0,
    Padded = 0x1,
    ShowContext = 0x2,
    ShowLineNumbers = 0x4
};

static inline int digits(int len)
{
    int ret = 1;
    while (len >= 10) {
        len /= 10;
        ++ret;
    }
    return ret;
}

ByteArray shortOptions(const option *longOptions);
int readLine(FILE *f, char *buf = 0, int max = -1);
bool removeDirectory(const char *path);
int canonicalizePath(char *path, int len);
ByteArray unescape(ByteArray command);

template <typename T> class Ptr : public std::tr1::shared_ptr<T>
{
public:
    Ptr(T *t = 0)
        : std::tr1::shared_ptr<T>(t)
    {}
    operator T*() const { return std::tr1::shared_ptr<T>::get(); }
};
bool startProcess(const Path &dotexe, const List<ByteArray> &dollarArgs);

void findApplicationDirPath(const char *argv0);
Path applicationDirPath();
}

#endif
