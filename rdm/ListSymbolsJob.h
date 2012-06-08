#ifndef ListSymbolsJob_h
#define ListSymbolsJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include "QueryMessage.h"
#include "Job.h"

class ListSymbolsJob : public Job
{
    Q_OBJECT
public:
    ListSymbolsJob(int i, const QueryMessage &query);
protected:
    virtual void execute();
private:
    const QByteArray string;
    const unsigned queryFlags;
};

#endif
