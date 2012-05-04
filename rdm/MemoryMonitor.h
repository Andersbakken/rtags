#ifndef MEMORYMONITOR_H
#define MEMORYMONITOR_H

#include <QtGlobal>

class MemoryMonitor
{
public:
    static quint64 usage();

private:
    MemoryMonitor();
};

#endif
