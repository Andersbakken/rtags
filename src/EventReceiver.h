#ifndef EVENTRECEIVER_H
#define EVENTRECEIVER_H

class Event;

class EventReceiver
{
public:
    EventReceiver() {}
    virtual ~EventReceiver() {}

protected:
    virtual void event(const Event* event) {}

private:
    friend class EventLoop;
};

#endif
