#ifndef GRParseJob_h
#define GRParseJob_h

#include "ThreadPool.h"
#include "Path.h"
#include "AbortInterface.h"
#include "SignalSlot.h"
#include "Location.h"

class GRParseJob : public ThreadPool::Job, public AbortInterface
{
public:
    enum Flag {
        None = 0x0,
        Dirty = 0x1
    };
    GRParseJob(const Path &path, unsigned flags);
    const Path &path() const { return mPath; }
    unsigned flags() const { return mFlags; }
    virtual void run();
    time_t parseTime() const { return mParseTime; }
    signalslot::Signal2<GRParseJob *, const Map<ByteArray, Map<Location, bool> > &> &finished() { return mFinished; }
private:
    signalslot::Signal2<GRParseJob *, const Map<ByteArray, Map<Location, bool> > &> mFinished;

    Map<ByteArray, Map<Location, bool> > mEntries;
    const Path mPath;
    const unsigned mFlags;
    time_t mParseTime;
};

#endif
