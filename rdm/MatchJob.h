#ifndef MatchJob_h
#define MatchJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include "QueryMessage.h"
#include "Job.h"

class MatchJob : public Job
{
    Q_OBJECT
public:
    MatchJob(const QByteArray& p, int i, QueryMessage::Type type, unsigned flags);
protected:
    void run();
private:
    const QByteArray partial;
    const QueryMessage::Type type;
    const unsigned keyFlags;
};

#endif
