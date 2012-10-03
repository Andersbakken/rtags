#ifndef ReferencesJob_h
#define ReferencesJob_h

#include "ByteArray.h"
#include "List.h"
#include "RTags.h"
#include "Job.h"
#include "Location.h"
#include <memory>

class CursorInfo;
class ReferencesJob : public Job
{
public:
    ReferencesJob(const Location &location, const QueryMessage &query, const std::shared_ptr<Project> &project);
    ReferencesJob(const ByteArray &symbolName, const QueryMessage &query, const std::shared_ptr<Project> &project);
protected:
    virtual void execute();
private:
    Set<Location> locations;
    const ByteArray symbolName;
};

#endif
