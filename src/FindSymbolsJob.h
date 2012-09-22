#ifndef FindSymbolsJob_h
#define FindSymbolsJob_h

#include <ByteArray.h>
#include <List.h>
#include "QueryMessage.h"
#include "Job.h"

class FindSymbolsJob : public Job
{
public:
    FindSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &project);
protected:
    virtual void run();
private:
    const ByteArray string;
};

#endif
