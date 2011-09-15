#include "EBus.h"
#include <QCoreApplication>
#include <QSettings>
#include <QDataStream>
#include <QEventLoop>
#include <QTimer>
#include <arpa/inet.h>

EBus::EBus(QObject *parent)
    : QObject(parent), m_socket(0), m_pending(-1), m_loop(0)
{
}

EBus::EBus(QTcpSocket* socket, QObject* parent)
    : QObject(parent), m_socket(socket), m_pending(-1), m_loop(0)
{
    connect(m_socket, SIGNAL(readyRead()), this, SLOT(readData()));
    connect(m_socket, SIGNAL(disconnected()), this, SIGNAL(disconnected()));
}


quint16 EBus::port()
{
    enum { DefaultPort = 6767 };
    QSettings settings(QSettings::IniFormat, QSettings::UserScope,
                       QCoreApplication::organizationName(), QCoreApplication::applicationName());
    return settings.value("daemonPort", DefaultPort).toUInt();
}

static inline bool isConnecting(const QTcpSocket* socket)
{
    switch (socket->state()) {
    case QTcpSocket::ConnectingState:
    case QTcpSocket::HostLookupState:
        return true;
    default:
        break;
    }
    return false;
}

bool EBus::waitForReply(int msec)
{
    Q_ASSERT(m_socket->state() == QTcpSocket::ConnectedState);

    m_loop = new QEventLoop;
    QTimer::singleShot(msec, m_loop, SLOT(quit()));
    m_loop->exec();
    delete m_loop;
    m_loop = 0;

    return !m_toread.isEmpty();
}

bool EBus::hasData() const
{
    return !m_toread.isEmpty();
}

void EBus::push(const QVariant &arg)
{
    m_towrite.append(arg);
}

void EBus::send()
{
    Q_ASSERT(m_socket->state() == QTcpSocket::ConnectedState);

    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << m_towrite.size();
        foreach(const QVariant& arg, m_towrite) {
            stream << arg;
        }
        m_toread.clear();
    }
    int size = htonl(data.size());
    m_socket->write(reinterpret_cast<char*>(&size), 4);
    m_socket->write(data);
}

int EBus::peek() const
{
    if (m_toread.isEmpty())
        return QVariant::Invalid;
    return m_toread.at(0).userType();
}

QVariant EBus::pop()
{
    if (m_toread.isEmpty())
        return QVariant();
    return m_toread.takeFirst();
}

void EBus::readData()
{
    Q_ASSERT(m_socket->state() == QTcpSocket::ConnectedState);

    m_data += m_socket->readAll();
    if (m_pending == -1) {
        if (m_data.size() < static_cast<int>(sizeof(int)))
            return;
        memcpy(&m_pending, m_data.constData(), sizeof(int));
        m_pending = ntohl(m_pending);
        m_data = m_data.mid(sizeof(int));
    }
    if (m_pending != -1 && m_data.size() < m_pending)
        return;
    QByteArray data = m_data.left(m_pending);
    m_data = m_data.mid(m_pending);

    QDataStream stream(data);
    int num;
    stream >> num;
    QVariant var;
    for (int i = 0; i < num; ++i) {
        stream >> var;
        m_toread << var;
    }

    if (m_loop)
        m_loop->quit();
    emit ready();
}

EBusDaemon::EBusDaemon(QObject *parent)
    : QObject(parent), m_server(0)
{
}

bool EBusDaemon::start()
{
    if (m_server)
        return true;
    m_server = new QTcpServer(this);
    const bool ok = m_server->listen(QHostAddress::Any, EBus::port());
    if (ok)
        connect(m_server, SIGNAL(newConnection()), this, SLOT(clientConnected()));
    else {
        delete m_server;
        m_server = 0;
    }
    return ok;
}

void EBusDaemon::clientConnected()
{
    for (;;) {
        QTcpSocket* client = m_server->nextPendingConnection();
        if (!client)
            break;
        connect(client, SIGNAL(disconnected()), this, SLOT(clientDisconnected()));
        EBus* ebus = new EBus(client, this);
        m_clients.append(ebus);

        emit ebusConnected(ebus);
    }
}

void EBusDaemon::clientDisconnected()
{
    QTcpSocket* socket = qobject_cast<QTcpSocket*>(sender());
    Q_ASSERT(socket);

    QList<EBus*>::iterator it = m_clients.begin();
    const QList<EBus*>::const_iterator end = m_clients.end();
    while (it != end) {
        if ((*it)->m_socket == socket) {
            if ((*it)->m_loop)
                (*it)->m_loop->quit();
            delete *it;
            m_clients.erase(it);
            return;
        }
        ++it;
    }
}
bool EBus::connect(int timeout)
{
    if (isConnected())
        return true;
    delete m_socket;
    m_socket = new QTcpSocket;
    m_socket->connectToHost(QHostAddress::LocalHost, port());
    connect(m_socket, SIGNAL(readyRead()), this, SLOT(readData()));
    connect(m_socket, SIGNAL(disconnected()), this, SIGNAL(disconnected()));
    if (!m_socket->waitForConnected(timeout)) {
        delete m_socket;
        m_socket = 0;
        return false;
    }
    return true;
}

bool EBus::isConnected() const
{
    return m_socket && m_socket->state() == QAbstractSocket::ConnectedState;
}
