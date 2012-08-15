#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <ByteArray.h>
#include <List.h>
#include <RTags.h>
#include "Job.h"
#include "Location.h"
#include <tr1/memory>

class CursorInfo;
class ReferencesJob : public Job
{
public:
    ReferencesJob(const Location &location, const QueryMessage &query, const shared_ptr<Project> &project);
    ReferencesJob(const ByteArray &symbolName, const QueryMessage &query, const shared_ptr<Project> &project);
protected:
    virtual void execute();
private:
    void process(ScopedDB &db, const Location &pos, const CursorInfo &cursorInfo);
    Set<Location> locations, references, additional;
    const ByteArray symbolName;
};

#endif
