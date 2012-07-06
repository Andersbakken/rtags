#ifndef StatusJob_h
#define StatusJob_h

#include <ByteArray.h>
#include <List.h>
#include "Job.h"

class Indexer;
class StatusJob : public Job
{
public:
    StatusJob(int i, const ByteArray &query, Indexer *indexer);
    static const char *delimiter;
protected:
    virtual void execute();
private:
    const ByteArray query;
    Indexer *mIndexer;
};

#endif
