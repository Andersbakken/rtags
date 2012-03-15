#ifndef CLIENT_H
#define CLIENT_H

#include "GccArguments.h"
#include "Path.h"
#include "QueryMessage.h"
#include <QByteArray>
#include <QHash>
#include <QList>
#include <QObject>

class Connection;
class Message;

class Client : public QObject
{
    Q_OBJECT
public:
    enum Flags { SkipParen = 0x1, NoContext = 0x2 }; // duplicated in QueryMessage

    Client(int flags = 0, QObject* parent = 0);

    void parseMakefile(const Path &path);
    void query(QueryMessage::Type type, const QByteArray& msg,
               const QHash<Path, QByteArray> &unsavedFiles);
private slots:
    void onSendComplete();
    void onNewMessage(Message* message);
    void onMakefileDone();
    void onMakefileReady(const GccArguments& args);

private:
    QList<QByteArray> mapPchToInput(const QList<QByteArray>& input);

private:
    Connection* m_conn;
    int m_flags;
    bool m_makeDone;
    QHash<QByteArray, QByteArray> m_pchs;
};

#endif
