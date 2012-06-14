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
    Client(const QByteArray &name, unsigned flags = 0, const QList<QByteArray> &rdmArgs = QList<QByteArray>(), QObject *parent = 0);
    enum Flag {
        None = 0x0,
        AutostartRdm = 0x1,
        RestartRdm = 0x2
    };

    QList<QByteArray> rdmArgs() const { return mRdmArgs; }
    unsigned flags() const { return mFlags; }
    template<typename T>
    void message(const T *msg);
    bool connectToServer();
private slots:
    void onDisconnected();
    void onNewMessage(Message *message);
private:
    void sendMessage(int id, const QByteArray& msg);
    Connection *mConn;
    unsigned mFlags;
    QList<QByteArray> mRdmArgs;
    QEventLoop mLoop;
    const QByteArray mName;
};

template<typename T>
void Client::message(const T *msg)
{
    sendMessage(msg->messageId(), msg->toByteArray());
}

#endif
