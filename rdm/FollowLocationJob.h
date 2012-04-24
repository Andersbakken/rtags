#ifndef FollowLocationJob_h
#define FollowLocationJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include "RTags.h"
#include "Job.h"

class FollowLocationJob : public Job
{
    Q_OBJECT
public:
    FollowLocationJob(int id, const RTags::Location &loc, unsigned flags);
    ~FollowLocationJob();
protected:
    void run();
private:
    const RTags::Location location;
    const unsigned flags;
};

#endif
