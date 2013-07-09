#include "Client.h"
#include "Server.h"
#include <rct/Connection.h>
#include <rct/EventLoop.h>
#include <rct/Log.h>
#include <rct/Messages.h>
#include <rct/Rct.h>
#include <unistd.h>

Client::Client()
    : mConnection(0), mSendComplete(false)
{
}

bool Client::connectToServer(const Path &path, int timeout)
{
    assert(!mConnection);
    mConnection = new Connection;
    if (!mConnection->connectToServer(path, timeout)) {
        delete mConnection;
        mConnection = 0;
    } else {
        mConnection->disconnected().connect(this, &Client::onDisconnected);
        mConnection->newMessage().connect(this, &Client::onNewMessage);
        mConnection->sendComplete().connect(this, &Client::onSendComplete);
    }
    return mConnection;
}

bool Client::send(const Message *msg, int timeout)
{
    mSendComplete = false;
    assert(msg);
    assert(mConnection);

    if (!mConnection->send(msg))
        return false;
    EventLoop::instance()->run(timeout);
    return mSendComplete;
}

void Client::onNewMessage(Message *message, Connection *)
{
    if (message->messageId() == ResponseMessage::MessageId) {
        const String response = static_cast<ResponseMessage*>(message)->data();
        if (!response.isEmpty()) {
            error("%s", response.constData());
            fflush(stdout);
        }
    } else {
        error("Unexpected message: %d", message->messageId());
    }
}

void Client::onSendComplete(Connection *)
{
    mSendComplete = true;
    EventLoop::instance()->exit();
}

void Client::onDisconnected(Connection *)
{
    if (mConnection) {
        mConnection->deleteLater();
        mConnection = 0;
        EventLoop::instance()->exit();
    }
}
