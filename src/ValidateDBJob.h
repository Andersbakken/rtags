#ifndef ValidateDBJob_h
#define ValidateDBJob_h

#include "Job.h"
#include "Set.h"
#include "Location.h"
#include <signalslot.h>

class ValidateDBJob : public Job
{
public:
    ValidateDBJob(const Path &root, const Set<Location> &prev);
    signalslot::Signal1<const Set<Location> &> &errors() { return mErrors; }
protected:
    virtual void execute();
private:
    const Path mRoot;
    const Set<Location> mPrevious;
    signalslot::Signal1<const Set<Location> &> mErrors;

};

#endif
