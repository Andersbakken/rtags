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
    bool stopTimer(int id);
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

class Timer
{
public:
    inline Timer()
        : mId(0), mInterval(0), mSingleShot(false), mUserData(0)
    {}

    inline int start(const shared_ptr<EventReceiver> &receiver, int interval, bool singleShot, void *userData = 0)
    {
        assert(receiver);
        assert(interval >= 0);
        stop();
        mId = receiver->startTimer(interval, singleShot, userData);
        mReceiver = receiver;
        return mId;
    }

    inline void stop()
    {
        shared_ptr<EventReceiver> receiver = mReceiver.lock();
        if (mId > 0 && receiver) {
            receiver->stopTimer(mId);
            mId = mInterval = -1;
            mUserData = 0;
            mSingleShot = false;
            mReceiver.reset();
        }
    }

    inline int restart()
    {
        shared_ptr<EventReceiver> receiver = mReceiver.lock();
        if (receiver && mInterval) {
            return start(receiver, mInterval, mSingleShot, mUserData);
        }
        return -1;
    }

    inline int id() const
    {
        return mId;
    }

    inline int interval() const
    {
        return mInterval;
    }
    inline bool singleShot() const
    {
        return mSingleShot;
    }
    inline void *userData()
    {
        return mUserData;
    }

    shared_ptr<EventReceiver> receiver() const
    {
        return mReceiver.lock();
    }

private:
    int mId, mInterval;
    bool mSingleShot;
    void *mUserData;
    weak_ptr<EventReceiver> mReceiver;
};

#endif
