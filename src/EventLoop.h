#ifndef EVENTLOOP_H
#define EVENTLOOP_H

#include "Mutex.h"
#include "WaitCondition.h"
#include "Thread.h"
#include <vector>

class Event;
class EventReceiver;

class EventLoop
{
public:
    EventLoop();
    ~EventLoop();

    static EventLoop* instance();

    enum { Read = 1, Write = 2 };
    typedef void(*FdFunc)(int, unsigned int, void*);

    // The following three functions are thread safe
    void addFileDescriptor(int fd, unsigned int flags, FdFunc callback, void* userData);
    void removeFileDescriptor(int fd);
    void postEvent(EventReceiver* object, Event* event);

    void run();
    void exit();

    pthread_t thread() const { return mThread; }
private:
    void handlePipe();
    void sendPostedEvents();

private:
    int mEventPipe[2];
    bool mQuit;

    Mutex mMutex;
    WaitCondition mCond;

    struct FdData {
        int fd;
        unsigned int flags;
        FdFunc callback;
        void* userData;
    };
    std::vector<FdData> mFdData;

    struct EventData {
        EventReceiver* receiver;
        Event* event;
    };
    std::vector<EventData> mEvents;

    static EventLoop* sInstance;

    pthread_t mThread;
};

class EventLoopThread : public Thread
{
public:
    EventLoopThread()
    {}

protected:
    void run()
    {
        EventLoop::instance()->run();
    }
};


#endif
