#ifndef Job_h
#define Job_h

#include <QtCore>
#include "AbortInterface.h"

class Job : public QObject, public QRunnable, public AbortInterface
{
    Q_OBJECT;
public:
    enum Flag {
        None = 0x0,
        WriteUnfiltered = 0x1
    };
    enum Priority {
        QueryJobPriority = 0,
        CompletionMatchJobPriority = 4
    };
    Job(int id, Priority priority, unsigned flags = None, QObject *parent = 0);
    ~Job();
    void setPathFilters(const QList<QByteArray> &filter, bool filterSystemIncludes);
    QList<QByteArray> pathFilters() const;
    int id() const { return mId; }
    void write(const QByteArray &out);
    unsigned flags() const { return mFlags; }
    bool filter(const QByteArray &val) const;
    virtual void run();
    virtual void execute() {}
    int priority() const { return mPriority; }
signals:
#if !defined(Q_MOC_RUN)
private:
#endif
    void complete(int id);
    void output(int id, const QByteArray &out);
private:
    const int mId;
    const int mFlags;
    const Priority mPriority;
    QList<QByteArray> mPathFilters;
    bool mFilterSystemIncludes;
};

#endif
