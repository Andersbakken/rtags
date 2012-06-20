#ifndef DumpJob_h
#define DumpJob_h

#include <QObject>
#include <ByteArray.h>
#include <List.h>
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
