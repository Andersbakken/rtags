#include "Client.h"
#include "Messages.h"
#include "Connection.h"
#include "MakefileParser.h"
#include "ResponseMessage.h"
#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QDebug>
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
    if (!mConn->connectToServer(QString::fromStdString(mName))) {
        if (mFlags & AutostartRdm) {
            QString cmd = QCoreApplication::arguments().value(0);
            const int lastSlash = cmd.lastIndexOf('/');
            if (lastSlash != -1) {
                cmd.replace(lastSlash + 1, cmd.size() - lastSlash - 1, "rdm");
            } else {
                cmd = "rdm";
            }
            error("trying to start rdm %s [%s]", qPrintable(cmd), RTags::join(mRdmArgs, " ").constData());
            if (RTags::startProcess(ByteArray(cmd.toStdString()), mRdmArgs)) {
                error("Started successfully");
                for (int i=0; i<5; ++i) {
                    if (mConn->connectToServer(QString::fromStdString(mName))) {
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
