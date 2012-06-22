#ifndef SIGNALSLOT_H
#define SIGNALSLOT_H

#include "FastDelegate/FastDelegate.h"
#include <vector>

namespace signalslot {

template<typename Delegate>
class SignalBase
{
public:
    SignalBase() {}

protected:
    bool disconnect(const Delegate& delegate)
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
        return false;
    }

protected:
    std::vector<Delegate> mDelegates;
};

class Signal0 : public SignalBase<fastdelegate::FastDelegate0<> >
{
public:
    typedef fastdelegate::FastDelegate0<> Type;
    
    Signal0() {}

    void operator()()
    {
        typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
        while (it != end) {
            (*it)();
            ++it;
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)())
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)())
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }   

    void disconnect()
    {
        SignalBase<Type>::mDelegates.clear();
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
        typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
        while (it != end) {
            (*it)(a1);
            ++it;
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }   

    void disconnect()
    {
        SignalBase<Type>::mDelegates.clear();
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
        typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
        while (it != end) {
            (*it)(a1, a2);
            ++it;
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1, Arg2))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1, Arg2))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }   

    void disconnect()
    {
        SignalBase<Type>::mDelegates.clear();
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
        typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
        while (it != end) {
            (*it)(a1, a2, a3);
            ++it;
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }   

    void disconnect()
    {
        SignalBase<Type>::mDelegates.clear();
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
        typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
        while (it != end) {
            (*it)(a1, a2, a3, a4);
            ++it;
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }   

    void disconnect()
    {
        SignalBase<Type>::mDelegates.clear();
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
        typename std::vector<Type>::const_iterator it = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator end = SignalBase<Type>::mDelegates.end();
        while (it != end) {
            (*it)(a1, a2, a3, a4, a5);
            ++it;
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4, Arg5))
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
    }

    template<typename Class>
    bool disconnect(Class* object, void (Class::*function)(Arg1, Arg2, Arg3, Arg4, Arg5))
    {
        return SignalBase<Type>::disconnect(fastdelegate::MakeDelegate(object, function));
    }   

    void disconnect()
    {
        SignalBase<Type>::mDelegates.clear();
    }
};

}

#endif
