#ifndef AbortInterface_h
#define AbortInterface_h

#include <QBasicAtomicInt>

class AbortInterface
{
public:
    AbortInterface() { mAborted = 0; }
    inline void abort() { mAborted.testAndSetRelaxed(0, 1); }
    inline bool isAborted() { return mAborted.fetchAndAddRelaxed(0); }
private:
    QBasicAtomicInt mAborted;
};

#endif
