#ifndef StatusJob_h
#define StatusJob_h

#include <QObject>
#include <QRunnable>
#include <QByteArray>
#include <QList>
#include "Job.h"

class StatusJob : public Job
{
    Q_OBJECT
public:
    StatusJob(int i, const QByteArray &query);
protected:
    virtual void execute();
private:
    const QByteArray query;
};

#endif
