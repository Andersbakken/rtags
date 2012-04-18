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
    Client(int flags = 0, QObject* parent = 0);
    enum Flag {
        None = 0x0,
        SkipParen = 0x01
    };

    void parseMakefile(const Path &path);
    void query(const QueryMessage &msg);
private slots:
    void onSendComplete();
    void onNewMessage(Message* message);
    void onMakefileDone();
    void onMakefileReady(const GccArguments& args);
private:
    QList<QByteArray> mapPchToInput(const QList<QByteArray>& input);
private:
    Connection* mConn;
    int mFlags;
    bool mMakeDone;
    QHash<QByteArray, QByteArray> mPchs;
};

#endif
