#ifndef FindSymbolsJob_h
#define FindSymbolsJob_h

#include <QObject>
#include <ByteArray.h>
#include <List.h>
#include "QueryMessage.h"
#include "Job.h"

class FindSymbolsJob : public Job
{
public:
    FindSymbolsJob(int i, const QueryMessage &query);
protected:
    virtual void execute();
private:
    const ByteArray string;
    const unsigned queryFlags;
};

#endif
