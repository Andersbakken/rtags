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
    std::vector<const SignalBase<Delegate>*> mSignals;
};

class Signal0 : public SignalBase<fastdelegate::FastDelegate0<> >
{
public:
    typedef fastdelegate::FastDelegate0<> Type;
    
    Signal0() {}

    void operator()()
    {
        typename std::vector<Type>::const_iterator delit = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator delend = SignalBase<Type>::mDelegates.end();
        while (delit != delend) {
            (*delit)();
            ++delit;
        }
        typename std::vector<const SignalBase<Type>*>::const_iterator sigit = SignalBase<Type>::mSignals.begin();
        const typename std::vector<const SignalBase<Type>*>::const_iterator sigend = SignalBase<Type>::mSignals.end();
        while (sigit != sigend) {
            (*const_cast<Signal0*>(reinterpret_cast<const Signal0*>(*sigit)))();
            ++sigit;
        }
    }

    template<typename Class>
    void connect(Class* object, void (Class::*function)())
    {
        SignalBase<Type>::mDelegates.push_back(fastdelegate::MakeDelegate(object, function));
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
        typename std::vector<Type>::const_iterator delit = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator delend = SignalBase<Type>::mDelegates.end();
        while (delit != delend) {
            (*delit)(a1);
            ++delit;
        }
        typename std::vector<const SignalBase<Type>*>::const_iterator sigit = SignalBase<Type>::mSignals.begin();
        const typename std::vector<const SignalBase<Type>*>::const_iterator sigend = SignalBase<Type>::mSignals.end();
        while (sigit != sigend) {
            (*const_cast<Signal1*>(reinterpret_cast<const Signal1*>(*sigit)))(a1);
            ++sigit;
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
        typename std::vector<Type>::const_iterator delit = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator delend = SignalBase<Type>::mDelegates.end();
        while (delit != delend) {
            (*delit)(a1, a2);
            ++delit;
        }
        typename std::vector<const SignalBase<Type>*>::const_iterator sigit = SignalBase<Type>::mSignals.begin();
        const typename std::vector<const SignalBase<Type>*>::const_iterator sigend = SignalBase<Type>::mSignals.end();
        while (sigit != sigend) {
            (*const_cast<Signal2*>(reinterpret_cast<const Signal2*>(*sigit)))(a1, a2);
            ++sigit;
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
        typename std::vector<Type>::const_iterator delit = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator delend = SignalBase<Type>::mDelegates.end();
        while (delit != delend) {
            (*delit)(a1, a2, a3);
            ++delit;
        }
        typename std::vector<const SignalBase<Type>*>::const_iterator sigit = SignalBase<Type>::mSignals.begin();
        const typename std::vector<const SignalBase<Type>*>::const_iterator sigend = SignalBase<Type>::mSignals.end();
        while (sigit != sigend) {
            (*const_cast<Signal3*>(reinterpret_cast<const Signal3*>(*sigit)))(a1, a2, a3);
            ++sigit;
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
        typename std::vector<Type>::const_iterator delit = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator delend = SignalBase<Type>::mDelegates.end();
        while (delit != delend) {
            (*delit)(a1, a2, a3, a4);
            ++delit;
        }
        typename std::vector<const SignalBase<Type>*>::const_iterator sigit = SignalBase<Type>::mSignals.begin();
        const typename std::vector<const SignalBase<Type>*>::const_iterator sigend = SignalBase<Type>::mSignals.end();
        while (sigit != sigend) {
            (*const_cast<Signal4*>(reinterpret_cast<const Signal4*>(*sigit)))(a1, a2, a3, a4);
            ++sigit;
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
        typename std::vector<Type>::const_iterator delit = SignalBase<Type>::mDelegates.begin();
        const typename std::vector<Type>::const_iterator delend = SignalBase<Type>::mDelegates.end();
        while (delit != delend) {
            (*delit)(a1, a2, a3, a4, a5);
            ++delit;
        }
        typename std::vector<const SignalBase<Type>*>::const_iterator sigit = SignalBase<Type>::mSignals.begin();
        const typename std::vector<const SignalBase<Type>*>::const_iterator sigend = SignalBase<Type>::mSignals.end();
        while (sigit != sigend) {
            (*const_cast<Signal5*>(reinterpret_cast<const Signal5*>(*sigit)))(a1, a2, a3, a4, a5);
            ++sigit;
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
        SignalBase<Type>::mDelegates.clear();
        SignalBase<Type>::mSignals.clear();
    }
};

}

#endif
