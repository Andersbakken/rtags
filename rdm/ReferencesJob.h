#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <QRunnable>
#include <QObject>
#include <ByteArray.h>
#include <QList>
#include <RTags.h>
#include "Job.h"
#include "Location.h"

class ReferencesJob : public Job
{
    Q_OBJECT
public:
    ReferencesJob(int id, const Location &location, unsigned queryFlags);
    ReferencesJob(int id, const ByteArray &symbolName, unsigned keyflags);
protected:
    virtual void execute();
private:
    QSet<Location> locations;
    const ByteArray symbolName;
    const unsigned flags;
};

#endif
