#ifndef MatchJob_h
#define MatchJob_h

#include <QRunnable>
#include <QObject>
#include <QByteArray>
#include <QList>
#include "QueryMessage.h"

class MatchJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    MatchJob(const QByteArray& p, int i, QueryMessage::Type type, unsigned flags);
signals:
    void complete(int id, const QList<QByteArray>&);

protected:
    void run();

private:
    const QByteArray partial;
    const int id;
    const QueryMessage::Type type;
    const unsigned keyFlags;
};

#endif
