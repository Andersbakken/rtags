#ifndef Database_h
#define Database_h

#include <QtCore>
#include <QtSql>

namespace Database
{
bool init(const QString &file);

enum LookupFlags {
    None = 0x000,
    // IncludeStartsWith = 0x001,
    // IncludeEndsWith = 0x002,
    IncludeContains = 0x004
};

enum LookupType {
    Definition,
    Declaration,
    Reference
};
    
struct Location {
    QFileInfo file;
    int line, column;
};

struct Result {
    QString symbolName, path;
    LookupType type;
    QList<Location> locations;
};

struct Filter {
    QRegExp regExp;
    QString text;
    enum Flag {
        None = 0x000,
        FilterOut = 0x001,
        ExactMatch = 0x002,
        RegExp = 0x004
    };
};

int addFile(const QFileInfo &file);
int fileId(const QFileInfo &file);
void addSymbol(const QString &symbolName, LookupType type, const Location &location);
Result lookup(const QString &symbolName, LookupType type, unsigned flags,
              const QList<Filter> &filters = QList<Filter>());
void remove(const QFileInfo &file);
enum CacheStatus {
    CacheInvalid = 0x0,
    SqlCacheValid = 0x1,
    AstCacheValid = 0x2
};

unsigned validateCache(const QFileInfo &file, const QByteArray &compilerOptions);
};

#endif
