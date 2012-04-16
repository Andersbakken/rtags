#ifndef PollJob_h
#define PollJob_h

#include <QObject>
#include <QRunnable>
#include <QByteArray>
#include <QList>
class Indexer;
class PollJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    PollJob(Indexer *indexer, int id);
signals:
    void complete(int id, const QList<QByteArray>& locations);
protected:
    void run();
private:
    Indexer *indexer;
    const int id;
};

#endif
