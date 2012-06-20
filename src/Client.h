#ifndef CLIENT_H
#define CLIENT_H

#include "GccArguments.h"
#include "Path.h"
#include "QueryMessage.h"
#include <ByteArray.h>
#include <Map.h>
#include <List.h>
#include <QObject>

class Connection;
class Message;

class Client : public QObject
{
    Q_OBJECT
public:
    Client(const ByteArray &name, unsigned flags = 0, const List<ByteArray> &rdmArgs = List<ByteArray>(), QObject *parent = 0);
    enum Flag {
        None = 0x0,
        AutostartRdm = 0x1,
        RestartRdm = 0x2
    };

    List<ByteArray> rdmArgs() const { return mRdmArgs; }
    unsigned flags() const { return mFlags; }
    template<typename T>
    void message(const T *msg);
    bool connectToServer();
private slots:
    void onDisconnected();
    void onNewMessage(Message *message);
private:
    void sendMessage(int id, const ByteArray& msg);
    Connection *mConn;
    unsigned mFlags;
    List<ByteArray> mRdmArgs;
    QEventLoop mLoop;
    const ByteArray mName;
};

template<typename T>
void Client::message(const T *msg)
{
    sendMessage(msg->messageId(), msg->toByteArray());
}

#endif
