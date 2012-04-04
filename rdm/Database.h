#ifndef DATABASE_H
#define DATABASE_H

#include <QByteArray>
#include <QList>
#include <QObject>

class QueryMessage;
struct DatabaseImpl;
class Database : public QObject
{
    Q_OBJECT
public:
    enum Type {
        Dependency,
        Symbol,
        SymbolName,
        FileInformation,
        DatabaseTypeCount
    };

    Database(QObject* parent = 0);
    ~Database();

    int followLocation(const QueryMessage &query);
    int referencesForLocation(const QueryMessage &query);
    int referencesForName(const QueryMessage &query);
    int recompile(const QueryMessage &query);
    int match(const QueryMessage &query);
    int dump(const QueryMessage &query);
    int status(const QueryMessage &query);

    static void setBaseDirectory(const QByteArray& base);
    static QByteArray databaseName(Type type);

signals:
    void complete(int id, const QList<QByteArray>& locations);

private:
    int nextId();
    DatabaseImpl* mImpl;

    static QByteArray sBase;
};

#endif
