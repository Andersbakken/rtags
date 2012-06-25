#include "MemoryMonitor.h"
#include <ByteArray.h>
#include <List.h>
#include <unistd.h>
#include <stdlib.h>

MemoryMonitor::MemoryMonitor()
{
}

#if defined(Q_OS_LINUX)
static inline uint64_t usageLinux()
{
    uint64_t total = 0;
    const pid_t pid = getpid();
    FILE* file = fopen(("/proc/" + ByteArray::number(pid) + "/smaps").constData(), "r");
    if (!file)
        return 0;

    enum { BufferSize = 4096 };
    int lastnewline = 0, lastread = BufferSize;
    char buffer[BufferSize];

    for (;;) {
        if (lastnewline) {
            memmove(buffer, buffer + lastnewline + 1, lastread - lastnewline);
            lastread = lastnewline + 1;
        }

        lastread = fread(buffer + BufferSize - lastread, 1, lastread, file);
        if (!lastread)
            break;

        const char* end = buffer + lastread;
        char* entry;
        int nextnewline = 0;
        for (;;) {
            entry = buffer + nextnewline;
            lastnewline = nextnewline;
            for (char* nl = entry; nl != end; ++nl) {
                if (*nl == '\n') {
                    nextnewline = (nl - buffer) + 1;
                    break;
                }
            }
            if (lastnewline == nextnewline)
                break;

            if (!strncmp("Private_Clean:", entry, 14))
                total += (atoll(entry + 14) * 1024);
            else if (!strncmp("Private_Dirty:", entry, 14))
                total += (atoll(entry + 14) * 1024);
        }
    }

    fclose(file);

    return total;
}
#elif defined(Q_OS_FREEBSD)
static inline uint64_t usageFreeBSD()
{
#warning "implement me"
    return 0;
}
#elif defined(Q_OS_MAC)
static inline uint64_t usageOSX()
{
#warning "implement me"
    return 0;
}
#endif

uint64_t MemoryMonitor::usage()
{
#if defined(Q_OS_LINUX)
    return usageLinux();
#elif defined(Q_OS_FREEBSD)
    return usageFreeBSD();
#elif defined(Q_OS_MAC)
    return usageOSX();
#else
#error "MemoryMonitor does not support this system"
#endif
}
