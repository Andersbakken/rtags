#include "Database.h"
#include <QSqlDatabase>
#include <QSqlQuery>
#include <QSqlError>
#include <QTextStream>
#include <QStringList>
#include <QVariant>
#include <QDateTime>

static bool exec(QSqlQuery &query, const QString &queryString = QString())
{
    if (!(queryString.isEmpty() ? query.exec() : !query.exec(queryString))) {
        qWarning("Can't execute query %s (%s)",
                 qPrintable(query.lastQuery()),
                 qPrintable(query.lastError().text()));
        return false;
    }
    return true;
}

namespace Database {
bool init(const QString &dbFile)
{
    QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE");
    db.setDatabaseName(dbFile);
    if (!db.open()) {
        qWarning("Can't open database");
        return false;
    }

    struct Statement {
        const char *a, *b;
    } const statements[] = {
        { "CREATE TABLE DatabaseInfo(version INT);", "INSERT INTO DatabaseInfo VALUES('1')" },
        { "CREATE TABLE File("
          "id INTEGER PRIMARY KEY,"
          "fileName TEXT,"
          "options TEXT,"
          "lastModified INTEGER,"
          "astFile TEXT);", 0 },
        { "CREATE TABLE Symbol("
          "id INTEGER PRIMARY KEY,"
          "name TEXT,"
          "path TEXT);", 0 },
        { "CREATE TABLE Location("
          "symbolId INTEGER,"
          "type INTEGER,"
          "fileId INTEGER,"
          "line INTEGER,"
          "column INTEGER,"
          "FOREIGN KEY(symbolId) REFERENCES Symbol(id),"
          "FOREIGN KEY(fileId) REFERENCES File(id));", 0 },
        { 0, 0 }
    };
    QSqlQuery query;
    for (int i=0; statements[i].a; ++i) {
        if (query.exec(QString::fromLocal8Bit(statements[i].a)) && statements[i].b) {
            query.exec(QString::fromLocal8Bit(statements[i].b));
        }
    }
    return true;
}

static inline bool extractLocation(QSqlQuery &query, int index, Location &loc)
{
    loc.file = query.value(index).toString();
    bool ok = true;
    loc.line = query.value(index + 1).toInt(&ok);
    if (!ok)
        return false;
    loc.column = query.value(index + 2).toInt(&ok);
    if (!ok)
        return false;
    return true;
}

Result lookup(const QString &fullSymbolName, LookupType type,
                                  unsigned lookupFlags, const QList<Filter> &filters)
{
    Result r;
    r.type = type;
    Q_ASSERT_X(filters.isEmpty(), __FUNCTION__, "Filters not implemented yet");
    QString q = QString::fromAscii("SELECT Symbol.name, Symbol.path, File.filename, Location.line, Location.column "
                                   "FROM(File, Symbol, Location) "
                                   "WHERE Location.type=? AND File.id=Location.fileId AND "
                                   "Symbol.id=Location.symbolId ");
    
    const int colonColon = fullSymbolName.lastIndexOf(QLatin1String("::"));
    QString symbolName, path;
    if (colonColon != -1) {
        symbolName = fullSymbolName.mid(colonColon + 2);
        path = fullSymbolName.left(colonColon);
    } else {
        symbolName = fullSymbolName;
    }
    if (!symbolName.isEmpty()) {
        q += QLatin1String("AND Symbol.name ");
        if (lookupFlags & IncludeContains) {
            q += QLatin1String("LIKE ");
        } else {
            q += QLatin1String("= ");
        }
        q += symbolName;
    }
    if (!path.isEmpty()) {
        q += QLatin1String("AND Symbol.path ");
        if (lookupFlags & IncludeContains) {
            q += QLatin1String("LIKE ");
        } else {
            q += QLatin1String("= ");
        }
        q += path;
    }
    q += QLatin1Char(';'); // ### is this necessary?
    QSqlQuery query;
    if (!query.prepare(q)) {
        qWarning("Can't prepare SQL statement %s [%s]",
                 qPrintable(q), qPrintable(query.lastError().text()));
        return r;
    }
    query.addBindValue(type);
    if (!exec(query))
        return r;
    bool first = true;
    while (query.next()) {
        if (first) {
            r.symbolName = query.value(0).toString();
            r.path = query.value(1).toString();
        }
        Location loc;
        if (!extractLocation(query, 2, loc)) {
            qWarning("Can't extract location from query");
            continue;
        }
        r.locations.append(loc);
    }
    return r;
}

void remove(const QFileInfo &file)
{
    QSqlQuery query;
    query.prepare("SELECT id, astFile FROM File WHERE fileName = ?");
    query.addBindValue(file.absoluteFilePath());
    if (!query.exec() || !query.next()) {
        qWarning("Can't get results for %s (%s)",
                 qPrintable(query.lastQuery()),
                 qPrintable(query.lastError().text()));
        return;
    }
    const int fileId = query.value(0).toInt();
    QFile::remove(query.value(1).toString()); // remove ast file
    query.prepare("SELECT DISTINCT symbolId FROM Location WHERE fileId=?");
    query.addBindValue(fileId);
    exec(query);
    QSqlQuery q;
    q.prepare("DELETE FROM Symbol WHERE id = :id");
    while (query.next()) {
        q.bindValue(QLatin1String(":id"), query.value(0).toInt());
        if (!exec(q))
            continue;
    }
    
    query.prepare("DELETE FROM Location WHERE fileId=?");
    query.addBindValue(fileId);
    exec(query);
    query.prepare("DELETE FROM File WHERE id=?");
    query.addBindValue(fileId);
    exec(query);
}

unsigned validateCache(const QFileInfo &file, const QByteArray &compilerOptions)
{
    if (!file.exists()) {
        remove(file);
        return CacheInvalid;
    }

    const QString absoluteFilePath = file.absoluteFilePath();

    QSqlQuery query;
    query.prepare("SELECT lastModified, options, astFile, id FROM File WHERE fileName = ?");
    query.addBindValue(absoluteFilePath);
    if (!exec(query) || !query.next()) {
        remove(file);
        return CacheInvalid;
    }
    if (file.lastModified() != query.value(0).toDateTime()) { // does this work or should I do time_t myself?
        remove(file);
        return CacheInvalid;
    }

    if (query.value(1).toByteArray() != compilerOptions) {
        remove(file);
        return CacheInvalid;
    }

    unsigned ret = SqlCacheValid;
    if (QFile::exists(query.value(2).toString())) {
        ret |= AstCacheValid;
    } else {
        const int fileId = query.value(3).toInt();
        query.prepare("UPDATE File SET astFile = NULL WHERE id = ?");
        query.addBindValue(fileId);
        exec(query);
    }
    return ret;
}

int fileId(const QFileInfo &file)
{
    QSqlQuery q;
    q.prepare("SELECT id FROM File WHERE fileName = ?;");
    q.addBindValue(file.absoluteFilePath());
    if (q.exec() && q.next()) {
        return q.value(0).toInt();
    } else {
        return 0;
    }
}

// int addSymbol(const QString &symbolName, LookupType type, const Location &location)
// {
//     const int fid = fileId(location.file);
//     Q_ASSERT_X(fid, __FUNCTION__,
//                qPrintable("File not in database " + location.file.absoluteFilePath()));
//     QSqlQuery query;
//     // query.addBindValue(
// }

// int addFile(const QFileInfo &file, const QString)
// {
//     QSqlQuery q;
//     q.prepare("INSERT INTO File(fileName) VALUES(?);");
//     q.addFile(file.absoluteFilePath());
//     if (exec(q)) {
//         return q.lastInsertId().toInt();
//     }
//     return 0;

// }

}
