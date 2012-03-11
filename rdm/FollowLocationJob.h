#ifndef FollowLocationJob_h
#define FollowLocationJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>

class FollowLocationJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    FollowLocationJob(const QByteArray& fn, int i, int l, int c);
    ~FollowLocationJob();

signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();

private:
    QByteArray fileName;
    int id, line, col;
};

#endif
