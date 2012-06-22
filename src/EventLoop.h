#ifndef EVENTLOOP_H
#define EVENTLOOP_H

#include "Mutex.h"
#include <vector>

class Event;
class EventReceiver;

class EventLoop
{
public:
    EventLoop();
    ~EventLoop();

    typedef void(*FdFunc)(int, void*);

    // add/removeFileDescriptor are not thread safe
    void addFileDescriptor(int fd, FdFunc callback, void* userData);
    void removeFileDescriptor(int fd);

    // postEvent is thread safe
    void postEvent(EventReceiver* object, Event* event);

    void run();
    void exit();

private:
    void handlePipe();
    void sendPostedEvents();

private:
    struct FdData {
        int fd;
        FdFunc callback;
        void* userData;
    };
    int mEventPipe[2];
    std::vector<FdData> mFdData;
    bool mQuit;

    Mutex mMutex;
    struct EventData {
        EventReceiver* receiver;
        Event* event;
    };
    std::vector<EventData> mEvents;
};

#endif
