#include "Thread.h"

Thread::Thread()
{
}

Thread::~Thread()
{
}

void* Thread::internalStart(void* arg)
{
    Thread* that = reinterpret_cast<Thread*>(arg);
    that->run();
    return 0;
}

void Thread::start()
{
    pthread_create(&mThread, NULL, internalStart, this);
}

void Thread::join()
{
    void* ret;
    pthread_join(mThread, &ret);
}
