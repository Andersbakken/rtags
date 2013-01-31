#include <rct/MemoryMonitor.h>
#include <rct/String.h>
#include <rct/List.h>
#include <rct/Log.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#ifdef OS_Darwin
# include "Mutex.h"
# include "MutexLocker.h"
# include <pthread.h>
# include <mach/mach_traps.h>
# include <mach/mach_init.h>
# include <mach/mach_port.h>
# include <mach/mach_vm.h>
#endif

MemoryMonitor::MemoryMonitor()
{
}

#if defined(OS_Linux)
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
    const pid_t pid = getpid();
    FILE* file = fopen(("/proc/" + String::number(pid) + "/smaps").constData(), "r");
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
static pthread_once_t mutexOnce = PTHREAD_ONCE_INIT;
static Mutex* mutex;

static void initMutex()
{
    mutex = new Mutex;
}

static inline uint64_t usageOSX()
{
    if (pthread_once(&mutexOnce, initMutex)) {
        // something bad happened
        error("MemoryMonitor::usageOSX: pthread_once failure %d %s\n",
              errno, strerror(errno));
        abort();
    }

    MutexLocker locker(mutex);

    int total = 0;

    kern_return_t kr;
    mach_vm_size_t vmsize;
    mach_vm_address_t address;
    vm_region_basic_info_data_64_t info;
    mach_msg_type_number_t info_count;
    memory_object_name_t object;

    const vm_map_t task = mach_task_self();
    const vm_region_flavor_t flavor = VM_REGION_BASIC_INFO_64;

    do {
        info_count = VM_REGION_BASIC_INFO_COUNT_64;
        kr = mach_vm_region(task, &address, &vmsize, flavor,
                            (vm_region_info_t)&info, &info_count, &object);
        if (kr == KERN_SUCCESS) {
            if (info.inheritance == VM_INHERIT_COPY) {
                total += vmsize;
            }
            address += vmsize;
        } else if (kr != KERN_INVALID_ADDRESS) {
            return 0;
        }
    } while (kr != KERN_INVALID_ADDRESS);

    return total;
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
