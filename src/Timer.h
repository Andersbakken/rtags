#ifndef Timer_h
#define Timer_h

#include <stdint.h>
#include <sys/time.h>

class Timer
{
public:
    Timer()
        : mStart(current())
    {
    }

    long int start()
    {
        return (mStart = current());
    }

    long int startTime() const
    {
        return mStart;
    }

    static long int current()
    {
        timeval t;
        ::gettimeofday(&t, 0);
        return (t.tv_sec * 1000) + (t.tv_usec / 1000);
    }

    long int elapsed() const
    {
        return current() - mStart;
    }

    long int restart()
    {
        const long int cur = current();
        const long int ret = cur - mStart;
        mStart = cur;
        return ret;
    }
private:
    long int mStart;
};
#endif
