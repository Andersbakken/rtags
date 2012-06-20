#ifndef MEMORYMONITOR_H
#define MEMORYMONITOR_H

#include <QtGlobal>

class MemoryMonitor
{
public:
    static uint64_t usage();

private:
    MemoryMonitor();
};

#endif
