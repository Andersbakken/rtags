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
    Client(const QByteArray &name, unsigned flags = 0, const QList<QByteArray> &extraFlags = QList<QByteArray>(),
           const QList<QByteArray> &rdmArgs = QList<QByteArray>(), QObject *parent = 0);
    enum Flag {
        None = 0x0,
        AutostartRdm = 0x1,
        RestartRdm = 0x2
    };

    QList<QByteArray> rdmArgs() const { return mRdmArgs; }
    unsigned flags() const { return mFlags; }
    bool parseMakefile(const Path &path, const QList<QByteArray> &args, bool wait);
    template<typename T>
    void message(const T *msg);
    QList<QByteArray> extraFlags() const { return mExtraFlags; }
    int sourceFileCount() const { return mSourceFileCount; }
    int pchCount() const { return mPchCount; }
    bool connectToServer();
private slots:
    void onDisconnected();
    void onSendComplete();
    void onNewMessage(Message *message);
    void onMakefileDone();
    void onMakefileReady(const GccArguments& args);
private:
    void sendMessage(int id, const QByteArray& msg);
    QList<QByteArray> mapPchToInput(const QList<QByteArray>& input);
    Connection *mConn;
    unsigned mFlags;
    bool mMakeDone;
    QHash<QByteArray, QByteArray> mPchs;
    const QList<QByteArray> mExtraFlags;
    int mSourceFileCount, mPchCount;
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
