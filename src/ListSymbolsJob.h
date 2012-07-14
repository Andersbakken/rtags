#ifndef ListSymbolsJob_h
#define ListSymbolsJob_h

#include <ByteArray.h>
#include <List.h>
#include "QueryMessage.h"
#include "Job.h"

class ListSymbolsJob : public Job
{
public:
    ListSymbolsJob(const QueryMessage &query);
protected:
    virtual void execute();
private:
    const ByteArray string;
};

#endif
