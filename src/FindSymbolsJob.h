#ifndef FindSymbolsJob_h
#define FindSymbolsJob_h

#include <rct/String.h>
#include <rct/List.h>
#include "QueryMessage.h"
#include "Job.h"

class FindSymbolsJob : public Job
{
public:
    FindSymbolsJob(const QueryMessage &query, const std::shared_ptr<Project> &project);
protected:
    virtual void execute();
private:
    const String string;
};

#endif
