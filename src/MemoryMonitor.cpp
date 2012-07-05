#include "MemoryMonitor.h"
#include <ByteArray.h>
#include <List.h>
#include <unistd.h>
#include <stdlib.h>

MemoryMonitor::MemoryMonitor()
{
}

#if defined(OS_Linux)
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
#elif defined(OS_FreeBSD)
static inline uint64_t usageFreeBSD()
{
#warning "implement me"
    return 0;
}
#elif defined(OS_Darwin)
static inline uint64_t usageOSX()
{
#warning "implement me"
    return 0;
}
#endif

uint64_t MemoryMonitor::usage()
{
#if defined(OS_Linux)
    return usageLinux();
#elif defined(OS_FreeBSD)
    return usageFreeBSD();
#elif defined(OS_Darwin)
    return usageOSX();
#else
#error "MemoryMonitor does not support this system"
#endif
}
