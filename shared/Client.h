#ifndef CLIENT_H
#define CLIENT_H

#ifdef BUILDING_RC
#include "GccArguments.h"
#endif
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
    Client(unsigned flags, const QList<QByteArray> &extraFlags,
           const QStringList &rdmArgs, QObject* parent = 0);
    enum Flag {
        None = 0x0,
        AutostartRdm = 0x1
    };

    QStringList rdmArgs() const { return mRdmArgs; }
    unsigned flags() const { return mFlags; }
#ifdef BUILDING_RC
    void parseMakefile(const Path &path);
#endif
    void query(const QueryMessage &msg);
    QList<QByteArray> extraFlags() const { return mExtraFlags; }
    int sourceFileCount() const { return mSourceFileCount; }
    int pchCount() const { return mPchCount; }
    bool connectToServer();
private slots:
    void onDisconnected();
    void onSendComplete();
    void onNewMessage(Message* message);
#ifdef BUILDING_RC
    void onMakefileDone();
    void onMakefileReady(const GccArguments& args);
#endif
private:
#ifdef BUILDING_RC
    QList<QByteArray> mapPchToInput(const QList<QByteArray>& input);
#endif
    Connection* mConn;
    unsigned mFlags;
    bool mMakeDone;
    QHash<QByteArray, QByteArray> mPchs;
    const QList<QByteArray> mExtraFlags;
    int mSourceFileCount, mPchCount;
    QStringList mRdmArgs;
};

#endif
