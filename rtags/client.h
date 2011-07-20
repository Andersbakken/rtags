#ifndef CLIENT_H
#define CLIENT_H

#include <QObject>

class DaemonInterface;

class Client : public QObject
{
    Q_OBJECT
public:
    Client(QObject* parent = 0);

    bool connect();
    bool connected() const;
    void startDaemon(const QStringList& args);

    QString exec(const QStringList& args);

private:
    bool m_connected;
    DaemonInterface* m_interface;
};

#endif
