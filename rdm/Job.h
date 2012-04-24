#ifndef Job_h
#define Job_h
#include <QtCore>

class Job : public QObject, public QRunnable
{
    Q_OBJECT;
public:
    Job(int id, QObject *parent = 0)
        : QObject(parent), mId(id)
    {}

    int id() const
    {
        return mId;
    }
    virtual void abort() {}
signals:
    void complete(int id, const QList<QByteArray> &out);
    void complete(int id, const QByteArray &out = QByteArray());
    void output(int id, const QList<QByteArray> &out);
    void output(int id, const QByteArray &out);
private:
    const int mId;
};

#endif
