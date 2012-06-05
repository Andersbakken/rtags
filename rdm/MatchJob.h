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
    enum { CompletionMatchJobId = -1 };
    MatchJob(int i, const QueryMessage &query);
    static MatchJob *createCompletionMatchJob();
protected:
    virtual void execute();
private:
    const QByteArray partial;
    const QueryMessage::Type type;
    const unsigned keyFlags;
    const unsigned queryFlags;
};

#endif
