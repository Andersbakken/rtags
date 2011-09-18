#include "Client.h"
#include "Utils.h"

#define CLIENT_TIMEOUT 30000

Client::Client(QObject *parent)
    : QObject(parent)
{
}

bool Client::connect(int timeout)
{
    return mEbus.connect(timeout);
}

bool Client::isConnected() const
{
    return mEbus.isConnected();
}

void Client::startDaemon(const QStringList &args)
{
    const QString path = QDir::currentPath();
    QProcess::startDetached(args.first(), QStringList() << QLatin1String("daemonize"), path);
}

QHash<QByteArray, QVariant> Client::exec(const QHash<QByteArray, QVariant>& dashArgs, const QList<QByteArray>& freeArgs)
{
    if (!isConnected())
        return QHash<QByteArray, QVariant>();

    mEbus.push(qVariantFromValue(dashArgs));
    mEbus.push(qVariantFromValue(freeArgs));
    mEbus.send();

    QElapsedTimer timer;
    timer.start();
    QHash<QByteArray, QVariant> ret;

    do {
        if (!mEbus.hasData())
            mEbus.waitForReply(1000);
        if (mEbus.hasData() || timer.elapsed() > CLIENT_TIMEOUT)
            break;
    } while (mEbus.isConnected());
    if (!mEbus.hasData()) {
        ret.insert("result", "Unable to read from EBus");
        return ret;
    }
    ret = mEbus.pop().value<QHash<QByteArray, QVariant> >();
    if (ret.isEmpty())
        ret.insert("result", "Timeout while waiting for response");
    return ret;
}
