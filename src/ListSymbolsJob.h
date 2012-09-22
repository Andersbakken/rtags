#ifndef ListSymbolsJob_h
#define ListSymbolsJob_h

#include <ByteArray.h>
#include <List.h>
#include "QueryMessage.h"
#include "Job.h"

class ListSymbolsJob : public Job
{
public:
    ListSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj);
protected:
    virtual void run();
private:
    const ByteArray string;
};

#endif
