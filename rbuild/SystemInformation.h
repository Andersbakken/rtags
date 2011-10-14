#ifndef SYSTEMINFORMATION_H
#define SYSTEMINFORMATION_H

#include <QObject>
#include <QList>
#include <QMutex>
#include <QByteArray>

class QEventLoop;

class SystemInformation : public QObject
{
    Q_OBJECT
public:
    SystemInformation(QObject *parent = 0);

    void init();

    QList<QByteArray> systemIncludes();

signals:
    void done();

private slots:
    void parseSystemIncludes();

private:
    QList<QByteArray> mSystemIncludes;
};

#endif // SYSTEMINFORMATION_H
