#ifndef EBUS_H
#define EBUS_H

#include <QVariant>
#include <QTcpSocket>
#include <QTcpServer>
#include <QObject>

class EBusDaemon;
class QEventLoop;

class EBus : public QObject
{
    Q_OBJECT
public:
    static quint16 port();

    EBus(QObject* parent = 0);

    using QObject::connect;
    bool connect(int timeout);
    bool isConnected() const;

    void push(const QVariant &arg);
    void send();

    bool hasData() const;
    int peek() const;
    QVariant pop();

    bool waitForReply(int msec);

signals:
    void ready();
    void disconnected();

private slots:
    void readData();

private:
    EBus(QTcpSocket* socket, QObject* parent);

    QTcpSocket* m_socket;
    QList<QVariant> m_towrite, m_toread;
    int m_pending;
    QByteArray m_data;

    QEventLoop* m_loop;

    friend class EBusDaemon;
};

class EBusDaemon : public QObject
{
    Q_OBJECT
public:
    EBusDaemon(QObject* parent = 0);

    bool start();

signals:
    void ebusConnected(EBus* ebus);

private slots:
    void clientConnected();
    void clientDisconnected();

private:
    QTcpServer* m_server;
    QList<EBus*> m_clients;
};

#endif // EBUS_H
