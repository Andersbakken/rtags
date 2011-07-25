#include "Database.h"
#include <QSqlDatabase>
#include <QSqlQuery>
#include <QSqlError>
#include <QTextStream>
#include <QStringList>
#include <QVariant>

Database::Database()
{
}

bool Database::init(const QString &dbFile)
{
    QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE");
    db.setDatabaseName(dbFile);
    if (!db.open()) {
        qWarning("Can't open database");
        return false;
    }
    QFile file("db.sql");
    file.open(QIODevice::ReadOnly);
    Q_ASSERT(file.isOpen());
    QTextStream ts(&file);
    const QStringList statements = ts.readAll().split(QLatin1String("###"), QString::SkipEmptyParts);
    QSqlQuery query;
    foreach(const QString &statement, statements) {
        if (!query.exec(statement)) {
            qWarning("Can't execute statement [%s] %s",
                     qPrintable(statement), qPrintable(query.lastError().text()));
            return false;
        }
    }
    return true;
}

static inline bool extractLocation(QSqlQuery &query, int index, Database::Location &loc)
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

Database::Result Database::lookup(const QString &fullSymbolName, LookupType type,
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
    if (!query.exec()) {
        qWarning("Can't execute query %s (%s)",
                 qPrintable(query.lastQuery()),
                 qPrintable(query.lastError().text()));
        return r;
    }
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

void Database::remove(const QFileInfo &file)
{
    QSqlQuery query;
    query.prepare("SELECT id FROM File WHERE fileName = ?");
    query.addBindValue(file.absoluteFilePath());
    if (!query.exec() || !query.next()) {
        qWarning("Can't get results for %s (%s)",
                 qPrintable(query.lastQuery()),
                 qPrintable(query.lastError().text()));
        return;
    }
    const int fileId = query.value(0).toInt();
    query.prepare("DELETE FROM Location WHERE fileId=?");
    query.addBindValue(fileId);
    if (!query.exec()) {
        qWarning("Can't execute query %s (%s)",
                 qPrintable(query.lastQuery()),
                 qPrintable(query.lastError().text()));
    }
}
