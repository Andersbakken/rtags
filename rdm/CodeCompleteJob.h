#ifndef CodeCompleteJob_h
#define CodeCompleteJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include <QHash>
#include <Path.h>

class CodeCompleteJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    CodeCompleteJob(const QByteArray& fn, int i, int l, int c, const QHash<Path, QByteArray> &unsaved);
    ~CodeCompleteJob();

signals:
    void complete(int id, const QList<QByteArray>& output);

protected:
    void run();

private:
    QByteArray fileName;
    int id, line, col;
    QHash<Path, QByteArray> unsavedFiles;
};


#endif
