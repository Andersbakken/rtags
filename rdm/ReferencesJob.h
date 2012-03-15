#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include <RTags.h>

class ReferencesJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    ReferencesJob(int id, const RTags::Location &location);
    ReferencesJob(int id, const QByteArray &symbolName);
signals:
    void complete(int id, const QList<QByteArray>& locations);

protected:
    void run();
    void runLocation();
    void runName();

private:
    const int id;
    const RTags::Location location;
    const QByteArray symbolName;
};

#endif
