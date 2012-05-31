#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include <RTags.h>
#include "Job.h"
#include "Location.h"

class ReferencesJob : public Job
{
    Q_OBJECT
public:
    ReferencesJob(int id, const Location &location, unsigned queryFlags);
    ReferencesJob(int id, const QByteArray &symbolName, unsigned keyflags);
protected:
    virtual void execute();
private:
    QSet<Location> locations;
    const QByteArray symbolName;
    const unsigned flags;
};

#endif
