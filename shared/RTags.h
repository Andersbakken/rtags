#ifndef RTags_h
#define RTags_h

#include <QByteArray>
#include <clang-c/Index.h>
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
QByteArray eatString(CXString str);
QByteArray join(const QList<QByteArray> &list, const QByteArray &sep = QByteArray());
QByteArray cursorToString(CXCursor cursor);
int readLine(FILE *f, char *buf, int max);
QByteArray context(const Path &path, unsigned offset, unsigned col);

bool makeLocation(const QByteArray &arg, Location *loc,
                  QByteArray *resolvedLocation = 0, const Path &cwd = Path());
void makeLocation(QByteArray &path, int line, int col);

enum MakeLocationFlag {
    IncludeContext = 0x1
};
QByteArray makeLocation(CXCursor cursor, unsigned flags = 0);
QByteArray makeLocation(const QByteArray &encodedLocation);
}

#endif
