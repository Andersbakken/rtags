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

Client::~Client()
{
    delete mConnection;
}

bool Client::connectToServer(const Path &path, int timeout)
{
    assert(!mConnection);
    mConnection = new Connection;
    if (!mConnection->connectToServer(path, timeout)) {
        delete mConnection;
        mConnection = 0;
    } else {
        mConnection->disconnected().connect(std::bind(&Client::onDisconnected, this, std::placeholders::_1));
        mConnection->newMessage().connect(std::bind(&Client::onNewMessage, this, std::placeholders::_1, std::placeholders::_2));
        mConnection->sendComplete().connect(std::bind(&Client::onSendComplete, this, std::placeholders::_1));
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
    EventLoop::eventLoop()->exec(timeout);
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
}

void Client::onDisconnected(Connection *)
{
    if (mConnection) {
        EventLoop::deleteLater(mConnection);
        mConnection = 0;
        EventLoop::eventLoop()->quit();
    }
}
