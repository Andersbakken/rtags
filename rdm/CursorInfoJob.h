#ifndef CursorInfoJob_h
#define CursorInfoJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include <RTags.h>

class CursorInfoJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    CursorInfoJob(int id, const RTags::Location &loc);
    ~CursorInfoJob();

signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();

private:
    const int id;
    const RTags::Location location;
};

#endif
