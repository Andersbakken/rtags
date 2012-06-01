#ifndef RTags_h
#define RTags_h

#include <QByteArray>
#include <Path.h>
#include <QDir>
#include <Log.h>
#include <stdio.h>
#include <assert.h>
#include <getopt.h>

namespace RTags {
enum UnitType { CompileC, CompileCPlusPlus, PchC, PchCPlusPlus };

static inline Path rtagsDir() { return (QDir::homePath() + "/.rtags/").toLocal8Bit(); }

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

QByteArray shortOptions(const option *longOptions);
int readLine(FILE *f, char *buf = 0, int max = -1);
bool removeDirectory(const char *path);
int canonicalizePath(char *path, int len);
QByteArray unescape(QByteArray command);
QByteArray join(const QList<QByteArray> &list, const QByteArray &sep = QByteArray());

template <typename T> class Ptr : public QScopedPointer<T>
{
public:
    Ptr(T *t = 0) : QScopedPointer<T>(t) {}
    operator T*() const { return QScopedPointer<T>::data(); }
};
bool startProcess(const Path &dotexe, const QList<QByteArray> &dollarArgs);
}

#endif
