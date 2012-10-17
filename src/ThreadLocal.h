#ifndef THREADLOCAL_H
#define THREADLOCAL_H

#include <pthread.h>
#include "Mutex.h"
#include "MutexLocker.h"

template<typename T>
class ThreadLocal
{
public:
    ThreadLocal() : mExists(false) {}
    ThreadLocal(const T& t) : mExists(false) { set(t); }
    ~ThreadLocal() { clear(); }

    void clear();

    void set(const T& t) { setData(new T(t)); }
    void remove() { setData(0); }
    T& get() { return *reinterpret_cast<T*>(getData()); }
    const T& get() const { return *reinterpret_cast<const T*>(getData()); }

    operator const T& () const { return get(); }
    operator T& () { return get(); }

private:
    void setData(void* data)
    {
        MutexLocker locker(&mMutex);
        if (mExists) {
            delete reinterpret_cast<T*>(getData());
        } else {
            mExists = true;
            pthread_key_create(&mKey, deleteValue);
        }
        pthread_setspecific(mKey, data);
    }
    void* getData() const
    {
        MutexLocker locker(&mMutex);
        if (!mExists) {
            mExists = true;
            pthread_key_create(&mKey, 0);
            T* empty = new T();
            pthread_setspecific(mKey, empty);
            return empty;
        }
        return pthread_getspecific(mKey);
    }
    static void deleteValue(void* val)
    {
        delete reinterpret_cast<T*>(val);
    }

private:
    mutable bool mExists;
    mutable Mutex mMutex;
    mutable pthread_key_t mKey;
};

template<typename T>
inline void ThreadLocal<T>::clear()
{
    MutexLocker locker(&mMutex);
    if (!mExists)
        return;
    pthread_key_delete(mKey);
    mExists = false;
}

#endif
