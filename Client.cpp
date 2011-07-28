#include "Client.h"
#ifdef DBUS_ENABLED
#include "DaemonInterface.h"
#endif
#include "Utils.h"

Client::Client(QObject *parent)
    : QObject(parent)
#ifdef EBUS_ENABLED
    , m_socket(0)
#else
    , m_interface(0)
#endif
{
    FUNC1(parent);
}

bool Client::connect()
{
    FUNC;
#ifdef EBUS_ENABLED
    using namespace EBus;
    delete m_socket;
    m_socket = new QTcpSocket(this);
    m_socket->connectToHost(QHostAddress::LocalHost, EBus::port()); // ### from settings
    return connected() || m_socket->waitForConnected(100); // slightly nasty
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
    return (m_socket && m_socket->state() == QAbstractSocket::ConnectedState);
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

QHash<QByteArray, QVariant> Client::exec(const QHash<QByteArray, QVariant>& a)
{
    FUNC1(a);
    if (!connected())
        return QHash<QByteArray, QVariant>();

    QHash<QByteArray, QVariant> args = a;
    args.insert("currentpath", QDir::currentPath());
#ifdef EBUS_ENABLED
    if (!EBus::writeToSocket(m_socket, args)) {
        QHash<QByteArray, QVariant> ret;
        ret.insert("result", "Couldn't write to socket");
        return ret;
    }
    QElapsedTimer timer;
    timer.start();
    qint16 size = -1;
    enum { SizeSize = sizeof(size) };
    QHash<QByteArray, QVariant> ret;

    do {
        if (!m_socket->bytesAvailable())
            m_socket->waitForReadyRead(1000);
        if (EBus::readFromSocket(m_socket, ret, size) == EBus::Error)
            ret.insert("result", "Read error " + m_socket->errorString());
    } while (ret.isEmpty());
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
