#ifndef DAEMON_H
#define DAEMON_H

#include <QObject>
#include <QString>
#include <QStringList>

namespace CPlusPlus {
class Snapshot;
}

class Daemon : public QObject
{
    Q_OBJECT
public:
    Daemon(QObject* parent = 0);
    ~Daemon();

    bool start();

private:
    QString addSourceFile(const QStringList& args);
    QString lookupLine(const QStringList& args);

private:
    Q_INVOKABLE QString runCommand(const QStringList& args);

private:
    CPlusPlus::Snapshot* m_snapshot;
};

#endif
