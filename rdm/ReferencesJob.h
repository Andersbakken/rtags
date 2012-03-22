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
    ReferencesJob(int id, const RTags::Location &location, bool includeContext);
    ReferencesJob(int id, const QByteArray &symbolName, bool includeContext);
signals:
    void complete(int id, const QList<QByteArray>& locations);
protected:
    void run();
private:
    const int id;
    RTags::Location location;
    const QByteArray symbolName;
    const bool includeContext;
};

#endif
