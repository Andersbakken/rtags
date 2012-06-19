#ifndef DumpJob_h
#define DumpJob_h

#include <QObject>
#include <QRunnable>
#include <ByteArray.h>
#include <QList>
#include "Job.h"

class DumpJob : public Job
{
    Q_OBJECT
public:
    DumpJob(const ByteArray& fn, int i);
protected:
    virtual void execute();
private:
    const ByteArray fileName;
};

#endif
