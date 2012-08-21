#ifndef EVENTRECEIVER_H
#define EVENTRECEIVER_H

#include "EventLoop.h"
#include "Event.h"

class EventReceiver
{
public:
    EventReceiver() {}
    virtual ~EventReceiver()
    {
        if (EventLoop *loop = EventLoop::instance())
            loop->removeEvents(this);
    }

    void postEvent(Event *event) // threadsafe
    {
        EventLoop::instance()->postEvent(this, event);
    }

    void deleteLater() { postEvent(new DeleteLaterEvent); }

protected:
    virtual void event(const Event* event)
    {
        if (event->type() == DeleteLaterEvent::Type)
            delete this;
    }

private:
    class DeleteLaterEvent : public Event
    {
    public:
        enum { Type = -1 };
        DeleteLaterEvent()
            : Event(Type)
        {}
    };

    friend class EventLoop;
};

#endif
