#ifndef FollowLocationJob_h
#define FollowLocationJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include "RTags.h"

class FollowLocationJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    FollowLocationJob(int id, const RTags::Location &loc);
    ~FollowLocationJob();

signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();

private:
    const int id;
    const RTags::Location location;
};

#endif
