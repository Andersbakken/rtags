#ifndef DumpJob_h
#define DumpJob_h

#include <QObject>
#include <QRunnable>
#include <QByteArray>
#include <QList>
#include "Job.h"

class DumpJob : public Job
{
    Q_OBJECT
public:
    DumpJob(const QByteArray& fn, int i);
protected:
    virtual void execute();
private:
    const QByteArray fileName;
};

#endif
