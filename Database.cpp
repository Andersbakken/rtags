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

static inline uint qHash(const QFileInfo &fi)
{
    return qHash(fi.absoluteFilePath());
}

static QHash<QFileInfo, int> s_fileIds;
static QHash<QByteArray, int> s_symbols;

static inline int fileId(const Database::Location &location)
{
    if (location.fileId > 0)
        return location.fileId;
    return Database::fileId(location.file);
}

namespace Database {
void clearMemoryCaches()
{
    s_fileIds.clear();
    s_symbols.clear();
}

bool init(const QString &dbFile)
{
    clearMemoryCaches(); // needs a removedatabase
    QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE");
    db.setDatabaseName(":memory:");
    // db.setDatabaseName(dbFile);
    if (!db.open()) {
        qWarning("Can't open database");
        return false;
    }

    enum { Version = 1 };
    const char *statements[] = {
        "CREATE TABLE DatabaseInfo(version INT);",

        "INSERT INTO DatabaseInfo VALUES(1)",

        "CREATE TABLE File("
        "id INTEGER PRIMARY KEY,"
        "fileName TEXT,"
        "options TEXT,"
        "lastModified INTEGER,"
        "astFile TEXT);",

        "CREATE TABLE Symbol("
        "id INTEGER PRIMARY KEY,"
        "fileId INTEGER," // declaration
        "line INTEGER," // declaration
        "column INTEGER," // declaration
        "name TEXT," // fully qualified e.g. namespace1::namespace2::Class::function
        "FOREIGN KEY(fileId) REFERENCES File(id));",

        "CREATE TABLE Reference(" // need primary key?
        "symbolId INTEGER,"
        "type INTEGER," // maps to LookupType (should never be 0|Declaration)
        "fileId INTEGER,"
        "line INTEGER,"
        "column INTEGER,"
        "FOREIGN KEY(symbolId) REFERENCES Symbol(id),"
        "FOREIGN KEY(fileId) REFERENCES File(id));",

        // "DELETE FROM Reference",
        // "DELETE FROM Symbol",
        // "DELETE FROM File",

        0
    };
    QSqlQuery query;
    for (int i=0; statements[i]; ++i) {
        if (!query.exec(QString::fromLocal8Bit(statements[i])))
            break;
    }
    if (!query.exec("SELECT version FROM DatabaseInfo") || !query.next()) {
        qWarning("Couldn't read version from database");
        return false;
    } else if (query.value(0).toInt() != Version) {
        qWarning("Incompatible database got %d, expected %d",
                 query.value(0).toInt(), Version);
        return false;
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

Result lookup(const QByteArray &symbolName, unsigned lookupFlags, const QList<Filter> &filters)
{
    const unsigned lookupTypes = lookupFlags & (Declaration|Definition|Reference);
    Q_ASSERT_X(lookupTypes, __FUNCTION__,
               "lookupFlags must contain at least one of Declaration, Definition or Reference");
    Result r;
    Q_ASSERT_X(filters.isEmpty(), __FUNCTION__, "Filters not implemented yet");
    QByteArray select;
    select = "SELECT Symbol.id, Symbol.name";
    QByteArray from = " FROM Symbol";
    QByteArray where;
    if (!symbolName.isEmpty()) {
        where = " Symbol.name ";
        if (lookupFlags & IncludeContains) {
            where += "LIKE '%" + symbolName + "%'";
        } else {
            where += "= " + symbolName;
        }
    }
    
    if (lookupTypes & Declaration) {
        select += ", File.fileName, Symbol.fileId, Symbol.line, Symbol.column";
        from += ", File";
        if (!where.isEmpty())
            where += " AND ";
        where += "File.id = Symbol.fileId";
    }

    if (!where.isEmpty())
        where.prepend(" WHERE");
    QString q = select + from + where;
    QSqlQuery query;
    if (!exec(query, q))
        return r;
    bool first = true;
    while (query.next()) {
        if (first)
            r.symbolName = query.value(0).toByteArray();
        Location loc;
        if (!extractLocation(query, 1, loc)) {
            qWarning("Can't extract location from query");
            continue;
        }
        // r.matches.append(loc);
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
    int &ref = s_fileIds[file];
    if (!ref) {
        QSqlQuery q;
        q.prepare("SELECT id FROM File WHERE fileName = ?;");
        q.addBindValue(file.absoluteFilePath());
        if (q.exec() && q.next()) {
            ref = q.value(0).toInt();
        }
    }
    return ref;
}

int addSymbolDeclaration(const QByteArray &symbolName, const Location &location)
{
    int fileId = ::fileId(location);
    if (!fileId) {
        fileId = addFile(location.file, QByteArray());
    }
    qDebug() << "adding symbol declaration" << symbolName << "at" << location;

    Q_ASSERT_X(fileId, __FUNCTION__,
               qPrintable("File not in database " + location.file.absoluteFilePath()));
    if (s_symbols.contains(symbolName)) {
        qDebug() << s_symbols.value(symbolName) << symbolName << location;
    }
    Q_ASSERT(!s_symbols.contains(symbolName));
    QSqlQuery query;
    query.prepare("INSERT INTO Symbol(fileId, line, column, name) "
                  "VALUES(?, ?, ?, ?);");
    query.addBindValue(fileId);
    query.addBindValue(location.line);
    query.addBindValue(location.column);
    query.addBindValue(symbolName);
    ::exec(query);
    int id = query.lastInsertId().toInt();
    if (id > 0)
        s_symbols[symbolName] = id;
    return id;
}

int addFile(const QFileInfo &file, const QByteArray &compilerOptions)
{
    Q_ASSERT(file.exists());
    const int existing = fileId(file);
    if (existing) {
        QSqlQuery q;
        q.prepare("UPDATE File SET options = ?");
        q.addBindValue(compilerOptions);
        if (exec(q)) {
            return existing;
        }
    } else {
        QSqlQuery q;
        q.prepare("INSERT INTO File(fileName, options) VALUES(?, ?);");
        q.addBindValue(file.absoluteFilePath());
        q.addBindValue(compilerOptions);
        if (exec(q)) {
            const int id = q.lastInsertId().toInt();
            s_fileIds[file] = id;
            return id;
        }
    }
    return 0;
}

int symbolId(const QByteArray &symbolName, Qt::MatchFlags flags)
{
    // if (flags == Qt::MatchExactly) {
    int ret = s_symbols.value(symbolName, 0);
    if (!ret) {
        QSqlQuery q;
        q.prepare("SELECT id FROM Symbol WHERE name = ?;");
        q.addBindValue(symbolName);
        if (q.exec() && q.next()) {
            ret = q.value(0).toInt();
            s_symbols[symbolName] = ret;
        }
    }
    return ret;
}

QByteArray symbolName(int symbolId)
{
    for (QHash<QByteArray, int>::const_iterator it = s_symbols.begin();
         it != s_symbols.end(); ++it) {
        if (it.value() == symbolId)
            return it.key();
    }
    QSqlQuery q;
    q.prepare("SELECT name FROM Symbol WHERE id = ?;");
    q.addBindValue(symbolId);
    if (q.exec() && q.next()) {
        const QByteArray name = q.value(0).toByteArray();
        s_symbols[name] = symbolId;
        return name;
    }
    return QByteArray();
}

static void addSymbol(int symbolId, LookupFlag type, const Location &location)
{
    const int fileId = ::fileId(location);
    Q_ASSERT_X(fileId, __FUNCTION__,
               qPrintable("File not in database " + location.file.absoluteFilePath()));
    
    QSqlQuery query;
    query.prepare("INSERT INTO Reference(symbolId, type, fileId, line, column) "
                  "VALUES(?, ?, ?, ?, ?);");
    query.addBindValue(symbolId);
    query.addBindValue(type);
    query.addBindValue(fileId);
    query.addBindValue(location.line);
    query.addBindValue(location.column);
    ::exec(query);
    qDebug() << "adding symbol" << symbolName(symbolId)
             << (type == Reference ? "Reference" : "Definition")
             << "at" << location;
}

void addSymbolDefinition(int symbolId, const Location &location)
{
    addSymbol(symbolId, Definition, location);
}

void addSymbolReference(int symbolId, const Location &location)
{
    addSymbol(symbolId, Reference, location);
}
}
