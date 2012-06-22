#ifndef EVENTLOOP_H
#define EVENTLOOP_H

#include "Mutex.h"
#include "WaitCondition.h"
#include <vector>

class Event;
class EventReceiver;

class EventLoop
{
public:
    EventLoop();
    ~EventLoop();

    static EventLoop* instance();

    typedef void(*FdFunc)(int, void*);

    // The following three functions are thread safe
    void addFileDescriptor(int fd, FdFunc callback, void* userData);
    void removeFileDescriptor(int fd);
    void postEvent(EventReceiver* object, Event* event);

    void run();
    void exit();

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
};

#endif
