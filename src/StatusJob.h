#ifndef StatusJob_h
#define StatusJob_h

#include <QObject>
#include <ByteArray.h>
#include <List.h>
#include "Job.h"

class StatusJob : public Job
{
public:
    StatusJob(int i, const ByteArray &query);
    static const char *delimiter;
protected:
    virtual void execute();
private:
    const ByteArray query;
};

#endif
