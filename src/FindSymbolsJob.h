#ifndef FindSymbolsJob_h
#define FindSymbolsJob_h

#include <ByteArray.h>
#include <List.h>
#include "QueryMessage.h"
#include "Job.h"

class FindSymbolsJob : public Job
{
public:
    FindSymbolsJob(const QueryMessage &query);
protected:
    virtual void execute();
private:
    const ByteArray string;
};

#endif
