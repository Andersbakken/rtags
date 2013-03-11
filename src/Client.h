#ifndef CLIENT_H
#define CLIENT_H

#include "GccArguments.h"
#include <rct/Path.h>
#include "QueryMessage.h"
#include <rct/String.h>
#include <rct/Map.h>
#include <rct/List.h>

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
        DontWarnOnConnectionFailure = 0x4
    };
    enum SendFlag {
        SendNone,
        SendDontRunEventLoop
    };

    List<String> rdmArgs() const { return mRdmArgs; }
    unsigned flags() const { return mFlags; }
    bool message(const Message *msg, SendFlag flag = SendNone);
    bool connectToServer();
    void onDisconnected(Connection *);
    void onNewMessage(Message *message, Connection *);
    static void initMessages();
private:
    const int mConnectTimeout;
    Connection *mConnection;
    unsigned mFlags;
    List<String> mRdmArgs;
    const Path mName;
};

#endif
