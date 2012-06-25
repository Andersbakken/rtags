#ifndef THREAD_H
#define THREAD_H

#include <pthread.h>

class Thread
{
public:
    Thread();
    virtual ~Thread();

    void start();
    void join();

    pthread_t self() const { return mThread; }
protected:
    virtual void run() = 0;

private:
    static void* internalStart(void* arg);

private:
    pthread_t mThread;
};

#endif
