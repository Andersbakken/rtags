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

    int id() const { return mId; }
    virtual void abort() {}
    void write(const QList<QByteArray> &out) { emit output(id(), out); }
    void write(const QByteArray &out) { emit output(id(), out); }
    void finish() { emit complete(id()); }
    void finish(const QByteArray &out) { emit complete(id(), out); }
    void finish(const QList<QByteArray> &out) { emit complete(id(), out); }
signals:
#if !defined(Q_MOC_RUN)
private:
#endif
    void complete(int id, const QList<QByteArray> &out);
    void complete(int id, const QByteArray &out);
    void complete(int id);
    void output(int id, const QList<QByteArray> &out);
    void output(int id, const QByteArray &out);
private:
    const int mId;
};

#endif
