#include "Client.h"
#include "Messages.h"
#include "Connection.h"
#include "MakefileParser.h"
#include "ResponseMessage.h"
#include <Log.h>
#include <unistd.h>

Client::Client(const ByteArray &name, unsigned flags, const List<ByteArray> &rdmArgs, QObject *parent)
    : QObject(parent), mConn(0), mFlags(flags), mRdmArgs(rdmArgs), mName(name)
{
    if ((mFlags & (RestartRdm|AutostartRdm)) == (RestartRdm|AutostartRdm)) {
        mFlags &= ~AutostartRdm; // this is implied and would upset connectToServer
    }
    Messages::init();
    const bool ret = connectToServer();
    if (mFlags & RestartRdm) { // ### something about this is buggy
        if (ret) {
            QueryMessage msg(QueryMessage::Shutdown);
            message(&msg);
            delete mConn;
            mConn = 0;
        }
        mFlags |= AutostartRdm;
        connectToServer();
        mFlags &= ~AutostartRdm;
    }
}

void Client::sendMessage(int id, const ByteArray &msg)
{
    if (!mConn && !connectToServer() && !(mFlags & (RestartRdm|AutostartRdm))) {
        if (!(mFlags & DontWarnOnConnectionFailure))
            error("Can't seem to connect to server");
        return;
    }

    connect(mConn, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    connect(mConn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
    mConn->send(id, msg);
    mLoop.exec();
}

void Client::onNewMessage(Message *message)
{
    Q_ASSERT(mConn == sender());
    if (message->messageId() == ResponseMessage::MessageId) {
        const ByteArray response = static_cast<ResponseMessage*>(message)->data();
        if (!response.isEmpty()) {
            printf("%s\n", response.constData());
        }
    } else {
        qFatal("Unexpected message: %d", message->messageId());
    }
    message->deleteLater();
}


void Client::onDisconnected()
{
    if (sender() == mConn) {
        mConn->deleteLater();
        mConn = 0;
        mLoop.quit();
    }
}
bool Client::connectToServer()
{
    Q_ASSERT(!mConn);
    mConn = new Connection(this);
    if (!mConn->connectToServer(mName)) {
        if (mFlags & AutostartRdm) {
            const Path cmd = RTags::applicationDirPath() + "/rdm";
            warning("trying to start rdm %s [%s]", cmd.nullTerminated(), ByteArray::join(mRdmArgs, " ").constData());
            if (RTags::startProcess(cmd, mRdmArgs)) {
                warning("Started successfully");
                for (int i=0; i<5; ++i) {
                    if (mConn->connectToServer(mName)) {
                        return true;
                    }
                    sleep(1);
                }
            } else {
                error("Couldn't start");
            }

        }

        warning("Can't connect to host");
        delete mConn;
        mConn = 0;
        return false;
    }
    return true;
}
