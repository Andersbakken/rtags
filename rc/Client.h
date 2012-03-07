#ifndef CLIENT_H
#define CLIENT_H

#include "GccArguments.h"
#include <QObject>
#include <QByteArray>
#include <QList>

class Connection;
class Message;

class Client : public QObject
{
    Q_OBJECT
public:
    enum QueryType { FollowLocation, ReferencesLocation, ReferencesName, Recompile, Match };
    enum Verbosity { Verbose, Silent };

    Client(Verbosity verbosity = Silent, QObject* parent = 0);

    void parseMakefile(const QByteArray& makefile);
    void query(QueryType type, const QByteArray& msg);

private slots:
    void onSendComplete();
    void onNewMessage(Message* message);
    void onMakefileDone();
    void onMakefileReady(const GccArguments& args);

private:
    Connection* m_conn;
    Verbosity m_verbosity;
    bool m_makeDone;
};

#endif
