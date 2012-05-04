#include "MemoryMonitor.h"
#include <QByteArray>
#include <QList>
#include <QFile>
#include <unistd.h>
#include <stdlib.h>

MemoryMonitor::MemoryMonitor()
{
}

#ifdef Q_OS_LINUX
static inline quint64 usageLinux()
{
    const pid_t pid = getpid();
    QFile file(("/proc/" + QByteArray::number(pid) + "/smaps"));
    if (!file.open(QFile::ReadOnly))
        return 0;
    quint64 total = 0;
    QList<QByteArray> lines = file.readAll().split('\n');
    foreach(const QByteArray& line, lines) {
        if (!qstrncmp("Private_Clean:", line, 14))
            total += (atoll(line.constData() + 14) * 1024);
        else if (!qstrncmp("Private_Dirty:", line, 14))
            total += (atoll(line.constData() + 14) * 1024);
    }
    return total;
}
#elif Q_OS_FREEBSD
static inline quint64 usageFreeBSD()
{
#warning "implement me"
    return 0;
}
#elif Q_OS_MAC
static inline quint64 usageOSX()
{
#warning "implement me"
    return 0;
}
#endif

quint64 MemoryMonitor::usage()
{
#ifdef Q_OS_LINUX
    return usageLinux();
#elif Q_OS_FREEBSD
    return usageFreeBsd();
#elif Q_OS_MAC
    return usageOSX();
#else
#error "MemoryMonitor does not support this system"
#endif
}
