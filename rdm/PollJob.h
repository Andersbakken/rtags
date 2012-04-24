#ifndef PollJob_h
#define PollJob_h

#include <QObject>
#include <QRunnable>
#include <QByteArray>
#include <QList>
#include "Job.h"
class Indexer;
class PollJob : public Job
{
    Q_OBJECT
public:
    PollJob(Indexer *indexer, int id);
protected:
    void run();
private:
    Indexer *indexer;
};

#endif
