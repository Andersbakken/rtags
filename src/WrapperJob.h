#ifndef WrapperJob_h
#define WrapperJob_h

#include <rct/ThreadPool.h>
#include <rct/Connection.h>
#include "Job.h"

typedef Job JobJob;

class WrapperJob : public ThreadPool::Job
{
public:
    WrapperJob(Connection *conn, const std::shared_ptr<JobJob> &job)
        : mConnection(conn), mJob(job)
    {
        assert(job);
    }

    virtual void run()
    {
        mJob->run(mConnection);
    }
private:
    Connection *mConnection;
    std::shared_ptr<JobJob> mJob;
};

#endif
