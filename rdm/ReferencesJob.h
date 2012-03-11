#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>

class ReferencesJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    ReferencesJob(const QByteArray& fn, int i, int l, int c);
    ~ReferencesJob();

signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();
    void runLocation();
    void runName();

private:
    QByteArray fileName;
    int id, line, col;
};

#endif
