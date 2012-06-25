#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <QObject>
#include <ByteArray.h>
#include <List.h>
#include <RTags.h>
#include "Job.h"
#include "Location.h"

class ReferencesJob : public Job
{
public:
    ReferencesJob(int id, const Location &location, unsigned queryFlags);
    ReferencesJob(int id, const ByteArray &symbolName, unsigned keyflags);
protected:
    virtual void execute();
private:
    Set<Location> locations;
    const ByteArray symbolName;
    const unsigned flags;
};

#endif
