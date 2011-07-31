#include "Client.h"
#ifdef DBUS_ENABLED
#include "DaemonInterface.h"
#endif
#include "Utils.h"

#ifdef EBUS_ENABLED
#define CLIENT_TIMEOUT 30000
#endif

Client::Client(QObject *parent)
    : QObject(parent)
#ifndef EBUS_ENABLED
    , m_interface(0)
#endif
{
    FUNC1(parent);
}

bool Client::connect()
{
    FUNC;
#ifdef EBUS_ENABLED
    return ebus.connected();
#else
    m_interface = new DaemonInterface(DaemonInterface::staticInterfaceName(), "/", QDBusConnection::sessionBus(), this);

    if (!m_interface->isValid()) {
        delete m_interface;
        m_interface = 0;
        return false;
    }

    return true;
#endif
}

bool Client::connected() const
{
    FUNC;
#ifdef EBUS_ENABLED
    return ebus.connected();
#else
    return (m_interface && m_interface->isValid());
#endif
}

void Client::startDaemon(const QStringList &args)
{
    FUNC1(args);
    const QString path = QDir::currentPath();
    QProcess::startDetached(args.first(), QStringList() << QLatin1String("--command=daemonize"), path);
}

QHash<QByteArray, QVariant> Client::exec(const QHash<QByteArray, QVariant>& dashArgs, const QList<QByteArray>& freeArgs)
{
    FUNC2(dashArgs, freeArgs);
    if (!connected())
        return QHash<QByteArray, QVariant>();

#ifdef EBUS_ENABLED
    ebus.push(qVariantFromValue(dashArgs));
    ebus.push(qVariantFromValue(freeArgs));
    ebus.send();

    QElapsedTimer timer;
    timer.start();
    QHash<QByteArray, QVariant> ret;

    do {
        if (!ebus.hasData())
            ebus.waitForReply(1000);
        if (ebus.hasData() || timer.elapsed() > CLIENT_TIMEOUT)
            break;
    } while (ebus.connected());
    if (!ebus.hasData()) {
        ret.insert("result", "Unable to read from EBus");
        return ret;
    }
    ret = ebus.pop().value<QHash<QByteArray, QVariant> >();
    if (ret.isEmpty())
        ret.insert("result", "Timeout while waiting for response");
    return ret;
#else
    QDBusPendingReply<QHash<QByteArray, QVariant>> reply = m_interface->runCommand(args);
    reply.waitForFinished();

    if (reply.isError()) {
        QHash<QByteArray, QVariant> ret;
        ret.insert("result", QDBusError::errorString(reply.error().type()) + ": " + reply.error().message());
        return ret;
    } else
        return reply.value();
#endif
}
