#ifndef CursorInfoJob_h
#define CursorInfoJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include "RTags.h"
#include "Job.h"

class CursorInfoJob : public Job
{
    Q_OBJECT
public:
    CursorInfoJob(int id, const RTags::Location &loc, unsigned flags);
    ~CursorInfoJob();
protected:
    void run();
private:
    const RTags::Location location;
    const unsigned flags;
};

#endif
