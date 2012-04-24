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
    Client(unsigned flags, const QList<QByteArray> &extraFlags, QObject* parent = 0);
    enum Flag {
        None = 0x0
    };

    unsigned flags() const { return mFlags; }
    void parseMakefile(const Path &path);
    void query(const QueryMessage &msg);
    QList<QByteArray> extraFlags() const { return mExtraFlags; }
private slots:
    void onDisconnected();
    void onSendComplete();
    void onNewMessage(Message* message);
    void onMakefileDone();
    void onMakefileReady(const GccArguments& args);
private:
    QList<QByteArray> mapPchToInput(const QList<QByteArray>& input);
private:
    Connection* mConn;
    unsigned mFlags;
    bool mMakeDone;
    QHash<QByteArray, QByteArray> mPchs;
    const QList<QByteArray> mExtraFlags;
};

#endif
