#ifndef CodeCompleteJob_h
#define CodeCompleteJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include <QHash>
#include <Path.h>
#include <RTags.h>

class CodeCompleteJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    CodeCompleteJob(int id, const RTags::Location &loc,
                    const QHash<Path, QByteArray> &unsaved);
    ~CodeCompleteJob();

signals:
    void complete(int id, const QList<QByteArray>& output);

protected:
    void run();

private:
    const int id;
    const RTags::Location location;
    const QHash<Path, QByteArray> unsavedFiles;
};


#endif
