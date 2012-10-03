#ifndef SIGNALSLOT_H
#define SIGNALSLOT_H

#include "FastDelegate.h"
#include <vector>
#include "Event.h"
#include "EventLoop.h"
class EventReceiver;
namespace signalslot {

template<typename Delegate>
class SignalBase
{
public:
    SignalBase() {}

protected:
    bool disconnect(const Delegate& delegate)
    {
        {
            typename std::vector<Delegate>::iterator it = mDelegates.begin();
            const typename std::vector<Delegate>::const_iterator end = mDelegates.end();
            while (it != end) {
                if (delegate == *it) {
                    mDelegates.erase(it);
                    return true;
                }
                ++it;
            }
        }
        {
            typename std::vector<std::pair<Delegate, EventReceiver*> >::iterator it = mEventDelegates.begin();
            const typename std::vector<std::pair<Delegate, EventReceiver*> >::const_iterator end = mEventDelegates.end();
            while (it != end) {
                if (delegate == it->first) {
                    mEventDelegates.erase(it);
                    return true;
                }
                ++it;
            }
        }

        return false;
    }

    bool disconnect(const SignalBase<Delegate>* signal)
    {
        typename std::vector<const SignalBase<Delegate>*>::iterator it = mSignals.begin();
        const typename std::vector<const SignalBase<Delegate>*>::const_iterator end = mSignals.end();
        while (it != end) {
            if (signal == *it) {
                mSignals.erase(it);
                return true;
            }
            ++it;
        }
        return false;
    }

protected:
    std::vector<Delegate> mDelegates;
    std::vector<std::pair<Delegate, EventReceiver*> > mEventDelegates;
    std::vector<const SignalBase<Delegate>*> mSignals;
};


class SignalEventBase : public Event
{
public:
    enum { Type = 1001 };
    SignalEventBase()
        : Event(Type)
    {}
    virtual void send() const = 0;
};

class SignalEvent0 : public SignalEventBase
{
public:
    SignalEvent0(const fastdelegate::FastDelegate0<> &delegate)
        : mDelegate(delegate)
    {}
    virtual void send() const
    {
        mDelegate();
    }
private:
    const fastdelegate::FastDelegate0<> mDelegate;
};

template <typename T, typename Arg1>
class SignalEvent1 : public SignalEventBase
{
public:
    SignalEvent1(const T &tt, const Arg1 arg)
        : t(tt), arg1(arg)
    {}
    virtual void send() const
    {
        t(arg1);
    }
private:
    const T t;
    const Arg1 arg1;
};

template <typename T, typename Arg1, typename Arg2>
class SignalEvent2 : public SignalEventBase
{
public:
    SignalEvent2(const T &tt, const Arg1 a1, const Arg2 a2)
        : t(tt), arg1(a1), arg2(a2)
    {}
    virtual void send() const
    {
        t(arg1, arg2);
    }
private:
    const T t;
    const Arg1 arg1;
    const Arg2 arg2;
};

template <typename T, typename Arg1, typename Arg2, typename Arg3>
class SignalEvent3 : public SignalEventBase
{
public:
    SignalEvent3(const T &tt, const Arg1 a1, const Arg2 a2, const Arg3 a3)
        : t(tt), arg1(a1), arg2(a2), arg3(a3)
    {}
    virtual void send() const
    {
        t(arg1, arg2, arg3);
    }
private:
    const T t;
    const Arg1 arg1;
    const Arg2 arg2;
    const Arg3 arg3;
};

template <typename T, typename Arg1, typename Arg2, typename Arg3, typename Arg4>
class SignalEvent4 : public SignalEventBase
{
public:
    SignalEvent4(const T &tt, const Arg1 a1, const Arg2 a2, const Arg3 a3, const Arg4 a4)
        : t(tt), arg1(a1), arg2(a2), arg3(a3), arg4(a4)
    {}
    virtual void send() const
    {
        t(arg1, arg2, arg3, arg4);
    }
private:
    const T t;
    const Arg1 arg1;
    const Arg2 arg2;
    const Arg3 arg3;
    const Arg4 arg4;
};

template <typename T, typename Arg1, typename Arg2, typename Arg3, typename Arg4, typename Arg5>
class SignalEvent5 : public SignalEventBase
{
public:
    SignalEvent5(const T &tt, const Arg1 a1, const Arg2 a2, const Arg3 a3, const Arg4 a4, const Arg5 a5)
        : t(tt), arg1(a1), arg2(a2), arg3(a3), arg4(a4), arg5(a5)
    {}
    virtual void send() const
    {
        t(arg1, arg2, arg3, arg4, arg5);
    }
private:
    const T t;
    const Arg1 arg1;
    const Arg2 arg2;
    const Arg3 arg3;
    const Arg4 arg4;
    const Arg5 arg5;
};

class Signal0 : public SignalBase<fastdelegate::FastDelegate0<> >
{
public:
    typedef fastdelegate::FastDelegate0<> Type;

    Signal0() {}

    void operator()()
    {
        {
            std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
            const std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
            while (it != end) {
                (*it)();
                ++it;
            }
        }
        {
            std::vector<std::pair<Type, EventReceiver*> >::const_iterator it = SignalBase<Type>::mEventDelegates.begin();
            const std::vector<std::pair<Type, EventReceiver*> >::const_iterator end = SignalBase<Type>::mEventDelegates.end();
            while (it != end) {
                if (EventLoop *loop = EventLoop::instance()) {
                    loop->postEvent(it->second, new SignalEvent0(it->first));
                }
                ++it;
            }
        }
        {
            std::vector<const SignalBase<Type>*>::const_iterator it = SignalBase<Type>::mSignals.begin();
            const std::vector<const SignalBase<Type>*>::const_iterator end = SignalBase<Type>::mSignals.end();
            while (it != end) {
                (*const_cast<Signal0*>(reinterpret_cast<const Signal0*>(*it)))();
                ++it;
            }
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)())
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    template<typename Class>
    void connectAsync(Class* object, void (Class::*function)())
    {
        assert(object);
        assert(EventLoop::instance());
        fastdelegate::FastDelegate0<> delegate = fastdelegate::MakeDelegate(object, function);
        SignalBase<Type>::mEventDelegates.push_back(std::make_pair(delegate, object));
    }

    void connect(const Signal0& signal)
    {
        SignalBase<Type>::mSignals.push_back(&signal);
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)())
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }

    bool disconnect(const Signal0& signal)
    {
        return SignalBase<Type>::disconnect(&signal);
    }

    void disconnect()
    {
        SignalBase<Type>::mEventDelegates.clear();
        SignalBase<Type>::mDelegates.clear();
        SignalBase<Type>::mSignals.clear();
    }
};

template<typename Arg1>
class Signal1 : public SignalBase<fastdelegate::FastDelegate1<Arg1> >
{
public:
    typedef fastdelegate::FastDelegate1<Arg1> Type;

    Signal1() {}

    void operator()(Arg1 a1)
    {
        {
            typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
            const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
            while (it != end) {
                (*it)(a1);
                ++it;
            }
        }
        {
            typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator it = SignalBase<Type>::mEventDelegates.begin();
            const typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator end = SignalBase<Type>::mEventDelegates.end();
            while (it != end) {
                if (EventLoop *loop = EventLoop::instance()) {
                    loop->postEvent(it->second, new SignalEvent1<Type, Arg1>(it->first, a1));
                }
                ++it;
            }
        }
        {
            typename std::vector<const SignalBase<Type>*>::const_iterator it = SignalBase<Type>::mSignals.begin();
            const typename std::vector<const SignalBase<Type>*>::const_iterator end = SignalBase<Type>::mSignals.end();
            while (it != end) {
                (*const_cast<Signal1*>(reinterpret_cast<const Signal1*>(*it)))(a1);
                ++it;
            }
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    void connect(const Signal1& signal)
    {
        SignalBase<Type>::mSignals.push_back(&signal);
    }

    template<typename Class>
    void connectAsync(Class* object, void (Class::*function)(Arg1))
    {
        assert(object);
        assert(EventLoop::instance());
        fastdelegate::FastDelegate1<Arg1> delegate = fastdelegate::MakeDelegate(object, function);
        SignalBase<Type>::mEventDelegates.push_back(std::make_pair(delegate, object));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }

    bool disconnect(const Signal1& signal)
    {
        return SignalBase<Type>::disconnect(&signal);
    }

    void disconnect()
    {
        SignalBase<Type>::mEventDelegates.clear();
        SignalBase<Type>::mDelegates.clear();
        SignalBase<Type>::mSignals.clear();
    }
};

template<typename Arg1, typename Arg2>
class Signal2 : public SignalBase<fastdelegate::FastDelegate2<Arg1, Arg2> >
{
public:
    typedef fastdelegate::FastDelegate2<Arg1, Arg2> Type;

    Signal2() {}

    void operator()(Arg1 a1, Arg2 a2)
    {
        {
            typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
            const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
            while (it != end) {
                (*it)(a1, a2);
                ++it;
            }
        }
        {
            typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator it = SignalBase<Type>::mEventDelegates.begin();
            const typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator end = SignalBase<Type>::mEventDelegates.end();
            while (it != end) {
                if (EventLoop *loop = EventLoop::instance()) {
                    loop->postEvent(it->second, new SignalEvent2<Type, Arg1, Arg2>(it->first, a1, a2));
                }
                ++it;
            }
        }
        {
            typename std::vector<const SignalBase<Type>*>::const_iterator it = SignalBase<Type>::mSignals.begin();
            const typename  std::vector<const SignalBase<Type>*>::const_iterator end = SignalBase<Type>::mSignals.end();
            while (it != end) {
                (*const_cast<Signal2*>(reinterpret_cast<const Signal2*>(*it)))(a1, a2);
                ++it;
            }
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1, Arg2))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    void connect(const Signal2& signal)
    {
        SignalBase<Type>::mSignals.push_back(&signal);
    }

    template<typename Class>
    void connectAsync(Class* object, void (Class::*function)(Arg1, Arg2))
    {
        assert(object);
        assert(EventLoop::instance());
        fastdelegate::FastDelegate2<Arg1, Arg2> delegate = fastdelegate::MakeDelegate(object, function);
        SignalBase<Type>::mEventDelegates.push_back(std::make_pair(delegate, object));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1, Arg2))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }

    bool disconnect(const Signal2& signal)
    {
        return SignalBase<Type>::disconnect(&signal);
    }

    void disconnect()
    {
        SignalBase<Type>::mEventDelegates.clear();
        SignalBase<Type>::mDelegates.clear();
        SignalBase<Type>::mSignals.clear();
    }
};

template<typename Arg1, typename Arg2, typename Arg3>
class Signal3 : public SignalBase<fastdelegate::FastDelegate3<Arg1, Arg2, Arg3> >
{
public:
    typedef fastdelegate::FastDelegate3<Arg1, Arg2, Arg3> Type;

    Signal3() {}

    void operator()(Arg1 a1, Arg2 a2, Arg3 a3)
    {
        {
            typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
            const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
            while (it != end) {
                (*it)(a1, a2, a3);
                ++it;
            }
        }
        {
            typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator it = SignalBase<Type>::mEventDelegates.begin();
            const typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator end = SignalBase<Type>::mEventDelegates.end();
            while (it != end) {
                if (EventLoop *loop = EventLoop::instance()) {
                    loop->postEvent(it->second, new SignalEvent3<Type, Arg1, Arg2, Arg3>(it->first, a1, a2, a3));
                }
                ++it;
            }
        }
        {
            typename std::vector<const SignalBase<Type>*>::const_iterator it = SignalBase<Type>::mSignals.begin();
            const typename std::vector<const SignalBase<Type>*>::const_iterator end = SignalBase<Type>::mSignals.end();
            while (it != end) {
                (*const_cast<Signal3*>(reinterpret_cast<const Signal3*>(*it)))(a1, a2, a3);
                ++it;
            }
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    void connect(const Signal3& signal)
    {
        SignalBase<Type>::mSignals.push_back(&signal);
    }

    template<typename Class>
    void connectAsync(Class* object, void (Class::*function)(Arg1, Arg2, Arg3))
    {
        assert(object);
        assert(EventLoop::instance());
        fastdelegate::FastDelegate3<Arg1, Arg2, Arg3> delegate = fastdelegate::MakeDelegate(object, function);
        SignalBase<Type>::mEventDelegates.push_back(std::make_pair(delegate, object));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }

    bool disconnect(const Signal3& signal)
    {
        return SignalBase<Type>::disconnect(&signal);
    }

    void disconnect()
    {
        SignalBase<Type>::mEventDelegates.clear();
        SignalBase<Type>::mDelegates.clear();
        SignalBase<Type>::mSignals.clear();
    }
};

template<typename Arg1, typename Arg2, typename Arg3, typename Arg4>
class Signal4 : public SignalBase<fastdelegate::FastDelegate4<Arg1, Arg2, Arg3, Arg4> >
{
public:
    typedef fastdelegate::FastDelegate4<Arg1, Arg2, Arg3, Arg4> Type;

    Signal4() {}

    void operator()(Arg1 a1, Arg2 a2, Arg3 a3, Arg4 a4)
    {
        {
            typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
            const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
            while (it != end) {
                (*it)(a1, a2, a3, a4);
                ++it;
            }
        }
        {
            typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator it = SignalBase<Type>::mEventDelegates.begin();
            const typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator end = SignalBase<Type>::mEventDelegates.end();
            while (it != end) {
                if (EventLoop *loop = EventLoop::instance()) {
                    loop->postEvent(it->second, new SignalEvent4<Type, Arg1, Arg2, Arg3, Arg4>(it->first, a1, a2, a3, a4));
                }
                ++it;
            }
        }
        {
            typename std::vector<const SignalBase<Type>*>::const_iterator it = SignalBase<Type>::mSignals.begin();
            const typename std::vector<const SignalBase<Type>*>::const_iterator end = SignalBase<Type>::mSignals.end();
            while (it != end) {
                (*const_cast<Signal4*>(reinterpret_cast<const Signal4*>(*it)))(a1, a2, a3, a4);
                ++it;
            }
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    void connect(const Signal4& signal)
    {
        SignalBase<Type>::mSignals.push_back(&signal);
    }

    template<typename Class>
    void connectAsync(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4))
    {
        assert(object);
        assert(EventLoop::instance());
        fastdelegate::FastDelegate4<Arg1, Arg2, Arg3, Arg4> delegate = fastdelegate::MakeDelegate(object, function);
        SignalBase<Type>::mEventDelegates.push_back(std::make_pair(delegate, object));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }

    bool disconnect(const Signal4& signal)
    {
        return SignalBase<Type>::disconnect(&signal);
    }

    void disconnect()
    {
        SignalBase<Type>::mEventDelegates.clear();
        SignalBase<Type>::mDelegates.clear();
        SignalBase<Type>::mSignals.clear();
    }
};

template<typename Arg1, typename Arg2, typename Arg3, typename Arg4, typename Arg5>
class Signal5 : public SignalBase<fastdelegate::FastDelegate5<Arg1, Arg2, Arg3, Arg4, Arg5> >
{
public:
    typedef fastdelegate::FastDelegate5<Arg1, Arg2, Arg3, Arg4, Arg5> Type;

    Signal5() {}

    void operator()(Arg1 a1, Arg2 a2, Arg3 a3, Arg4 a4, Arg5 a5)
    {
        {
            typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
            const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
            while (it != end) {
                (*it)(a1, a2, a3, a4, a5);
                ++it;
            }
        }
        {
            typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator it = SignalBase<Type>::mEventDelegates.begin();
            const typename std::vector<std::pair<Type, EventReceiver*> >::const_iterator end = SignalBase<Type>::mEventDelegates.end();
            while (it != end) {
                if (EventLoop *loop = EventLoop::instance()) {
                    loop->postEvent(it->second, new SignalEvent5<Type, Arg1, Arg2, Arg3, Arg4, Arg5>(it->first, a1, a2, a3, a4, a5));
                }
                ++it;
            }
        }
        {
            typename std::vector<const SignalBase<Type>*>::const_iterator it = SignalBase<Type>::mSignals.begin();
            const typename std::vector<const SignalBase<Type>*>::const_iterator end = SignalBase<Type>::mSignals.end();
            while (it != end) {
                (*const_cast<Signal5*>(reinterpret_cast<const Signal5*>(*it)))(a1, a2, a3, a4, a5);
                ++it;
            }
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4, Arg5))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    void connect(const Signal5& signal)
    {
        SignalBase<Type>::mSignals.push_back(&signal);
    }

    template<typename Class>
    void connectAsync(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4, Arg5))
    {
        assert(object);
        assert(EventLoop::instance());
        fastdelegate::FastDelegate5<Arg1, Arg2, Arg3, Arg4, Arg5> delegate = fastdelegate::MakeDelegate(object, function);
        SignalBase<Type>::mEventDelegates.push_back(std::make_pair(delegate, object));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4, Arg5))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }

    bool disconnect(const Signal5& signal)
    {
        return SignalBase<Type>::disconnect(&signal);
    }

    void disconnect()
    {
        SignalBase<Type>::mEventDelegates.clear();
        SignalBase<Type>::mDelegates.clear();
        SignalBase<Type>::mSignals.clear();
    }
};

}

#endif
