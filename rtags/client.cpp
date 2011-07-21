#include "client.h"
#include "daemoninterface.h"
#include <QProcess>
#include <QDir>

Client::Client(QObject *parent)
    : QObject(parent), m_connected(false), m_interface(0)
{
}

bool Client::connect()
{
    m_interface = new DaemonInterface(DaemonInterface::staticInterfaceName(), "/", QDBusConnection::sessionBus(), this);

    if (!m_interface->isValid()) {
        delete m_interface;
        m_interface = 0;
        return false;
    }

    return true;
}

bool Client::connected() const
{
    return (m_interface && m_interface->isValid());
}

void Client::startDaemon(const QStringList &args)
{
    QString path = QDir::currentPath();
    QProcess::startDetached(args.first(), QStringList() << QLatin1String("daemonize"), path);
}

QString Client::exec(const QStringList& a)
{
    if (!connected())
        return QString();

    QStringList args = a;
    args.removeFirst();
    args.prepend(QDir::currentPath());

    QDBusPendingReply<QString> reply = m_interface->runCommand(args);
    reply.waitForFinished();

    if (reply.isError())
        return QDBusError::errorString(reply.error().type()) + QLatin1String(": ") + reply.error().message();
    else
        return reply.value();
}
