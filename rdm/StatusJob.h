#ifndef StatusJob_h
#define StatusJob_h

#include <QObject>
#include <QRunnable>
#include <QByteArray>
#include <QList>

class StatusJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    StatusJob(int i, const QByteArray &query);
signals:
    void complete(int id, const QList<QByteArray>& locations);
protected:
    void run();
private:
    const int id;
    const QByteArray query;
};

#endif
