#ifndef CLIENT_H
#define CLIENT_H

#include "GccArguments.h"
#include "Path.h"
#include "QueryMessage.h"
#include "String.h"
#include "Map.h"
#include "List.h"

class Connection;
class Message;

class Client
{
public:
    Client(const Path &path, int timeout, unsigned flags = 0, const List<String> &rdmArgs = List<String>());
    enum Flag {
        None = 0x0,
        AutostartRdm = 0x1,
        RestartRdm = 0x2,
        DontWarnOnConnectionFailure = 0x4,
        DontInitMessages = 0x8
    };
    enum SendFlag {
        SendNone,
        SendDontRunEventLoop
    };

    List<String> rdmArgs() const { return mRdmArgs; }
    unsigned flags() const { return mFlags; }
    template<typename T>
    bool message(const T *msg, SendFlag flag = SendNone);
    bool connectToServer();
    void onDisconnected();
    void onNewMessage(Message *message, Connection *);
private:
    bool sendMessage(int id, const String& msg, SendFlag flag);
    const int mConnectTimeout;
    Connection *mConnection;
    unsigned mFlags;
    List<String> mRdmArgs;
    const Path mName;
};

template<typename T>
bool Client::message(const T *msg, SendFlag flag)
{
    return sendMessage(msg->messageId(), msg->encode(), flag);
}

#endif
