#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include <RTags.h>
#include "Job.h"

class ReferencesJob : public Job
{
    Q_OBJECT
public:
    ReferencesJob(int id, const RTags::Location &location, unsigned keyflags);
    ReferencesJob(int id, const QByteArray &symbolName, unsigned keyflags);
protected:
    void run();
private:
    QSet<RTags::Location> locations;
    const QByteArray symbolName;
    const unsigned keyFlags;
};

#endif
