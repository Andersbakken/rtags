#ifndef PokeJob_h
#define PokeJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>

class PokeJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    PokeJob(const QByteArray& fn, int i);
    ~PokeJob();

protected:
    void run();

private:
    QByteArray fileName;
    int id;
};

#endif
