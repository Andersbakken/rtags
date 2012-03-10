#ifndef CLIENT_H
#define CLIENT_H

#include "GccArguments.h"
#include "QueryMessage.h"
#include <QObject>
#include <QByteArray>
#include <QList>

class Connection;
class Message;

class Client : public QObject
{
    Q_OBJECT
public:
    enum Flags { Verbose = 0x1, SkipParen = 0x2 };

    Client(int flags = 0, QObject* parent = 0);

    void parseMakefile(const Path &path);
    void query(QueryMessage::Type type, const QByteArray& msg);

private slots:
    void onSendComplete();
    void onNewMessage(Message* message);
    void onMakefileDone();
    void onMakefileReady(const GccArguments& args);

private:
    Connection* m_conn;
    int m_flags;
    bool m_makeDone;
};

#endif
