#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <ByteArray.h>
#include <List.h>
#include <RTags.h>
#include "Job.h"
#include "Location.h"

class ReferencesJob : public Job
{
public:
    ReferencesJob(int id, const Location &location, const QueryMessage &query);
    ReferencesJob(int id, const ByteArray &symbolName, const QueryMessage &query);
protected:
    virtual void execute();
private:
    void process(ScopedDB &db, const Location &loc, Set<Location> &refs, Set<Location> *additionalReferences);
    Set<Location> locations;
    const ByteArray symbolName;
};

#endif
