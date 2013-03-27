#ifndef CLIENT_H
#define CLIENT_H

#include "GccArguments.h"
#include <rct/Path.h>
#include "QueryMessage.h"
#include <rct/String.h>
#include <rct/Map.h>
#include <rct/List.h>
#include <rct/Connection.h>

class Message;

class Client
{
public:
    Client();
    bool connectToServer(const Path &path, int connectTimeout);
    bool send(const Message *msg, int timeOut);
    Connection *connection() const { return mConnection; }
private:
    void onDisconnected(Connection *);
    void onNewMessage(Message *message, Connection *);
    void onSendComplete(Connection *);

    Connection *mConnection;
    bool mSendComplete, mConnected;
};

#endif
