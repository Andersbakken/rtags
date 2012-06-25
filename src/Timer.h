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

    uint64_t start()
    {
        return (mStart = current());
    }

    uint64_t startTime() const
    {
        return mStart;
    }

    static uint64_t current()
    {
        timeval t;
        ::gettimeofday(&t, 0);
        return t.tv_sec + (t.tv_usec / 1000);
    }

    uint64_t elapsed() const
    {
        return current() - mStart;
    }

    uint64_t restart()
    {
        const uint64_t cur = current();
        const uint64_t ret = cur - mStart;
        mStart = cur;
        return ret;
    }
private:
    uint64_t mStart;
};
#endif
