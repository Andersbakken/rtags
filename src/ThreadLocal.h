#ifndef THREADLOCAL_H
#define THREADLOCAL_H

#include <pthread.h>

// ### the const usage in this class is a bit hacky
template<typename T>
class ThreadLocal
{
public:
    ThreadLocal() : mExists(false) {}
    ThreadLocal(const T& t) : mExists(false) { set(t); }
    ~ThreadLocal() { clear(); }

    void clear() { if (!mExists) return; delete reinterpret_cast<T*>(getData()); pthread_key_delete(mKey); mExists = false; }

    void set(const T& t) { clear(); setData(new T(t)); }
    T& get() { return *reinterpret_cast<T*>(getData()); }
    const T& get() const { return *reinterpret_cast<const T*>(getData()); }

    operator const T& () { return get(); }

private:
    void ensureKey() const { if (mExists) return; mExists = true; pthread_key_create(&mKey, 0); }
    void setData(void* data) { ensureKey(); pthread_setspecific(mKey, data); }
    void* getData() const { if (!mExists) { ensureKey(); T* null = new T(); pthread_setspecific(mKey, null); return null; } return pthread_getspecific(mKey); }

private:
    mutable bool mExists;
    mutable pthread_key_t mKey;
};

#endif
