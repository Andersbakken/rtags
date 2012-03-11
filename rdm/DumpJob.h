#ifndef DumpJob_h
#define DumpJob_h

#include <QObject>
#include <QRunnable>
#include <QByteArray>
#include <QList>

class DumpJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    DumpJob(const QByteArray& fn, int i);
signals:
    void complete(int id, const QList<QByteArray>& locations);
protected:
    void run();
private:
    QByteArray fileName;
    int id;
};

#endif
