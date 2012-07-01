#ifndef EVENTRECEIVER_H
#define EVENTRECEIVER_H

class Event;
#include "EventLoop.h"

class EventReceiver
{
public:
    EventReceiver() {}
    virtual ~EventReceiver() {}

    void postEvent(Event *event) // threadsafe
    {
        EventLoop::instance()->postEvent(this, event);
    }
protected:
    virtual void event(const Event* event) {}

private:
    friend class EventLoop;
};

#endif
