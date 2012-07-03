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

    int start()
    {
        return (mStart = current());
    }

    int startTime() const
    {
        return mStart;
    }

    static int current()
    {
        timeval t;
        ::gettimeofday(&t, 0);
        return (t.tv_sec * 1000) + (t.tv_usec / 1000);
    }

    int elapsed() const
    {
        return current() - mStart;
    }

    int restart()
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
