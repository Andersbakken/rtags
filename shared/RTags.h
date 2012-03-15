#ifndef RTags_h
#define RTags_h

#include <QByteArray>
#include <Path.h>
#include <Log.h>
#include <stdio.h>
#include <assert.h>

namespace RTags {

enum UnitType { CompileC, CompileCPlusPlus, PchC, PchCPlusPlus };

struct Location {
    Location() : line(0), column(0), offset(-1) {}

    Path path;
    int line, column, offset;
};

int canonicalizePath(char *path, int len);
static inline int digits(int len)
{
    int ret = 1;
    while (len >= 10) {
        len /= 10;
        ++ret;
    }
    return ret;
}

QByteArray unescape(QByteArray command);
QByteArray join(const QList<QByteArray> &list, const QByteArray &sep = QByteArray());
int readLine(FILE *f, char *buf, int max);
QByteArray context(const Path &path, unsigned offset, unsigned col);

bool makeLocation(const QByteArray &arg, Location *loc,
                  QByteArray *resolvedLocation = 0, const Path &cwd = Path());
void makeLocation(QByteArray &path, int line, int col);

QByteArray makeLocation(const QByteArray &encodedLocation);
}

#endif
