#ifndef EVENTRECEIVER_H
#define EVENTRECEIVER_H

#include "EventLoop.h"
#include "Event.h"


class EventReceiver;
class DeleteInEventLoopEvent : public Event
{
public:
    enum { Type = -1 };
    DeleteInEventLoopEvent()
        : Event(Type)
    {}
};
class EventReceiver
{
public:
    EventReceiver() {}
    virtual ~EventReceiver() {}

    void postEvent(Event *event) // threadsafe
    {
        EventLoop::instance()->postEvent(this, event);
    }
    void deleteInEventLoop() { postEvent(new DeleteInEventLoopEvent); }
protected:
    virtual void event(const Event* event)
    {
        if (event->type() == DeleteInEventLoopEvent::Type)
            delete this;
    }

private:
    friend class EventLoop;
};

#endif
