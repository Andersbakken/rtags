#ifndef Daemon_p_h
#define Daemon_p_h

#include <QElapsedTimer>
#define DEBUG_FUNCTION_CALLS
#ifdef DEBUG_FUNCTION_CALLS
class Timer : public QElapsedTimer
{
public:
    Timer(const char *func)
        : mFunc(func)
    {
        qDebug("%s called", func);
        start();
    }
    ~Timer()
    {
        const int e = elapsed();
        qDebug("%s returns (%d ms)", mFunc, e);
    }
private:
    const char *mFunc;
};
#define FUNC Timer noCollisions(__FUNCTION__);
#else
#define FUNC
#endif

#endif
