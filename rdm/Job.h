#ifndef Job_h
#define Job_h

#include <QtCore>

class Job : public QObject, public QRunnable
{
    Q_OBJECT;
public:
    enum Flag {
        None = 0x0,
        WriteUnfiltered = 0x1
    };
    Job(int id, unsigned flags = None, QObject *parent = 0);
    void setPathFilters(const QList<QByteArray> &filter, bool filterSystemIncludes);
    QList<QByteArray> pathFilters() const;
    int id() const { return mId; }
    virtual void abort() {}
    void write(const QByteArray &out);
    void finish();
    unsigned flags() const { return mFlags; }
    bool filter(const QByteArray &val) const;
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
    const int mFlags;
    QList<QByteArray> mPathFilters;
    bool mFilterSystemIncludes;
};

#endif
