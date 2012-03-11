#ifndef RecompileJob_h
#define RecompileJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>

class RecompileJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    RecompileJob(const QByteArray& fn, int i);
    ~RecompileJob();

signals:
    void complete(int id, const QList<QByteArray>&);

protected:
    void run();

private:
    QByteArray fileName;
    int id;
};

#endif
