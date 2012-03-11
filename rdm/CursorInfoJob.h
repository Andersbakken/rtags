#ifndef CursorInfoJob_h
#define CursorInfoJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>

class CursorInfoJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    CursorInfoJob(const QByteArray& fn, int i, int l, int c);
    ~CursorInfoJob();

signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();

private:
    QByteArray fileName;
    int id, line, col;
};

#endif
