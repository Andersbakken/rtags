#ifndef StatusJob_h
#define StatusJob_h

#include <ByteArray.h>
#include <List.h>
#include "Job.h"

class Indexer;
class StatusJob : public Job
{
public:
    StatusJob(int i, const ByteArray &query, std::tr1::shared_ptr<Indexer> indexer);
    static const char *delimiter;
protected:
    virtual void execute();
private:
    const ByteArray query;
    std::tr1::shared_ptr<Indexer> mIndexer;
};

#endif
