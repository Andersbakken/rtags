#include "client.h"
#include "daemoninterface.h"
#ifdef EBUS
#include "utils.h"
#endif

Client::Client(QObject *parent)
    : QObject(parent)
#ifdef EBUS
    , m_socket(0)
#else
    , m_interface(0)
#endif
{
}

bool Client::connect()
{
#ifdef EBUS
    if (!m_socket) {
        m_socket = new QTcpSocket(this);
        m_socket->connectToHost(QHostAddress::LocalHost, ::port()); // ### from settings
    }
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
#ifdef EBUS
    return (m_socket && m_socket->state() == QAbstractSocket::ConnectedState);
#else
    return (m_interface && m_interface->isValid());
#endif
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
#ifdef EBUS
    if (!::writeToSocket(m_socket, args)) {
        return QLatin1String("Couldn't write to socket");
    }
    QElapsedTimer timer;
    timer.start();
    qint16 size = -1;
    enum { SizeSize = sizeof(size) };
    QEventLoop loop;
    QString ret;

    while (timer.elapsed() < 10000 && ret.isEmpty()) {
        m_socket->waitForReadyRead(100);
        if (::readFromSocket(m_socket, ret, size) == Error) {
            ret = "Read error";
        }
    }
    if (ret.isEmpty())
        ret = "Timeout while waiting for response";
    return ret;
#else
    QDBusPendingReply<QString> reply = m_interface->runCommand(args);
    reply.waitForFinished();

    if (reply.isError())
        return QDBusError::errorString(reply.error().type()) + QLatin1String(": ") + reply.error().message();
    else
        return reply.value();
#endif
}
