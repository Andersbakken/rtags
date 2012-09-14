#include "MemoryMonitor.h"
#include <ByteArray.h>
#include <List.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

MemoryMonitor::MemoryMonitor()
{
}

typedef bool (*LineVisitor)(char*, void*);
static void visitLine(FILE* stream, LineVisitor visitor, void* userData)
{
    enum { BufferSize = 4096 };
    char buffer[BufferSize];
    char* r;

    while (!feof(stream)) {
        r = fgets(buffer, BufferSize, stream);
        if (r) {
            if (!visitor(r, userData))
                return;
        }
    }
}

#if defined(OS_Linux)
struct

static bool lineVisitor(char* line, void* userData)
{
    int* total = static_cast<int*>(userData);
    if (!strncmp("Private_Clean:", line, 14))
        *total += (atoll(line + 14) * 1024);
    else if (!strncmp("Private_Dirty:", line, 14))
        *total += (atoll(line + 14) * 1024);
    return true;
}

static inline uint64_t usageLinux()
{
    uint64_t total = 0;
    const pid_t pid = getpid();
    FILE* file = fopen(("/proc/" + ByteArray::number(pid) + "/smaps").constData(), "r");
    if (!file)
        return 0;

    int total = 0;
    visitLine(file, lineVisitor, &total);

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
struct VisitorData
{
    bool regionFound;
    int total;
};

static bool lineVisitor(char* line, void* userData)
{
    VisitorData* data = static_cast<VisitorData*>(userData);

    if (!strncmp("REGION TYPE", line, 11))
        data->regionFound = true;
    if (data->regionFound && !strncmp("TOTAL", line, 5)) {
        data->total = (atof(line + 5) * (1024 * 1024));
        return false;
    }

    return true;
}

static inline uint64_t usageOSX()
{
    const pid_t pid = getpid();
    char buf[64];
    snprintf(buf, 64, "/usr/bin/vmmap %d", pid);
    FILE* p = popen(buf, "r");
    if (!p)
        return 0;

    VisitorData data = { false, 0 };
    visitLine(p, lineVisitor, &data);

    pclose(p);

    return data.total;
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
