#ifndef Database_h
#define Database_h

#include <QtCore>
#include <QtSql>

namespace Database
{
bool init(const QString &file);

enum LookupFlag {
    None = 0x000,
    // IncludeStartsWith = 0x001,
    // IncludeEndsWith = 0x002,
    IncludeContains = 0x004,
    Declaration = 0x008,
    Definition = 0x010,
    Reference = 0x020
};

struct Location {
    QFileInfo file;
    int line, column, fileId; // either fileId or file must be valid
};

static inline QDebug operator<<(QDebug dbg, const Location &loc)
{
    dbg << loc.file.absoluteFilePath() << "line" << loc.line
        << "column" << loc.column << "fileId" << loc.fileId;
    return dbg;
}

struct Result {
    QByteArray symbolName;
    QList<QPair<Location, LookupFlag> > matches;
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

void clearMemoryCaches();
int addFile(const QFileInfo &file, const QByteArray &compilerOptions);
int fileId(const QFileInfo &file);
bool removeFile(int fileId);

int addSymbol(const QByteArray &symbolName, const Location &location);
int symbolId(const QByteArray &symbolName, Qt::MatchFlags = Qt::MatchExactly);
void addSymbolDefinition(int symbolId, const Location &location);
void addSymbolReference(int symbolId, const Location &location);

Result lookup(const QByteArray &symbolName, unsigned flags,
              const QList<Filter> &filters = QList<Filter>());
enum CacheStatus {
    CacheInvalid = 0x0,
    SqlCacheValid = 0x1,
    AstCacheValid = 0x2
};

unsigned validateCache(const QFileInfo &file, const QByteArray &compilerOptions);
};

#endif
