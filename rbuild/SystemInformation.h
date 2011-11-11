#ifndef SYSTEMINFORMATION_H
#define SYSTEMINFORMATION_H

#include <QList>
#include <QByteArray>

class SystemInformation
{
public:
    SystemInformation();
    void init();
    QList<QByteArray> systemIncludes() const;
private:
    QList<QByteArray> mSystemIncludes;
};

#endif // SYSTEMINFORMATION_H
