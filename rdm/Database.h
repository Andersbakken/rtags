#ifndef DATABASE_H
#define DATABASE_H

#include <QObject>
#include <QByteArray>
#include <QList>

class DatabaseImpl;

class Database : public QObject
{
    Q_OBJECT
public:
    enum Type { Include, Definition, Reference, Symbol };

    Database(QObject* parent = 0);
    ~Database();

    int followLocation(const QByteArray& query);
    int referencesForLocation(const QByteArray& query);
    int referencesForName(const QByteArray& query);
    int recompile(const QByteArray& query);
    int match(const QByteArray& query);

    static void setBaseDirectory(const QByteArray& base);
    static QByteArray databaseName(Type type);

signals:
    void complete(int id, const QList<QByteArray>& locations);

private:
    DatabaseImpl* m_impl;

    static QByteArray s_base;
};

#endif
