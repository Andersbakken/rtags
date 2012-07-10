#include "Thread.h"

Thread::Thread()
    : mAutoDelete(false), mThread(0)
{
}

Thread::~Thread()
{
}

void* Thread::internalStart(void* arg)
{
    Thread* that = reinterpret_cast<Thread*>(arg);
    that->run();
    if (that->isAutoDelete())
        delete that;
    return 0;
}

void Thread::start()
{
    pthread_create(&mThread, NULL, internalStart, this);
}

bool Thread::join()
{
    void* ret;
    return !pthread_join(mThread, &ret);
}
