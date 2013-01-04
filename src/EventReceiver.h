#ifndef EVENTRECEIVER_H
#define EVENTRECEIVER_H

#include "EventLoop.h"
#include "Event.h"
#include "SignalSlot.h"

class EventReceiver : public enable_shared_from_this<EventReceiver>
{
public:
    EventReceiver();
    virtual ~EventReceiver();
    void postEvent(Event *event); // threadsafe
    void deleteLater();
    class TimerEvent
    {
    public:
        TimerEvent() : mId(0), mInterval(0), mSingleShot(false), mUserData(0) {}
        void stop() { mSingleShot = true; }
        inline int id() const { return mId; }
        inline int interval() const { return mInterval; }
        inline bool singleShot() const { return mSingleShot; }
        inline void *userData() const { return mUserData; }
    private:
        int mId, mInterval;
        bool mSingleShot;
        void *mUserData;
        friend class EventReceiver;
    };
    int startTimer(int interval, bool singleShot, void *userData = 0);
protected:
    virtual void timerEvent(TimerEvent *event);
    virtual void event(const Event *event);

private:
    void sendEvent(const Event *event);

    class DeleteLaterEvent : public Event
    {
    public:
        enum { Type = -1 };
        DeleteLaterEvent()
            : Event(Type)
        {}
    };

    Map<int, TimerEvent> mTimers;
    static void timerEventCallBack(int id, void *userData);
    friend class EventLoop;
};

#endif
