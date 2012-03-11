#ifndef MatchJob_h
#define MatchJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>

class MatchJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    MatchJob(const QByteArray& p, int i);
    ~MatchJob();

signals:
    void complete(int id, const QList<QByteArray>&);

protected:
    void run();

private:
    QByteArray partial;
    int id;
};

#endif
