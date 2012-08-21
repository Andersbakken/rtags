#ifndef EVENTRECEIVER_H
#define EVENTRECEIVER_H

#include "EventLoop.h"
#include "Event.h"
#include "SignalSlot.h"

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
        switch (event->type()) {
        case DeleteLaterEvent::Type:
            delete this;
            break;
        case signalslot::SignalEventBase::Type:
            static_cast<const signalslot::SignalEventBase*>(event)->send();
            break;
        }
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
