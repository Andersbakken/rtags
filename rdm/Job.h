#ifndef Job_h
#define Job_h

#include <QtCore>
#include "AbortInterface.h"
#include "ThreadPool.h"

class Job : public QObject, public ThreadPool::Job, public AbortInterface
{
    Q_OBJECT;
public:
    enum Flag {
        None = 0x0,
        WriteUnfiltered = 0x1,
        QuoteOutput = 0x2
    };
    enum Priority {
        QueryJobPriority = 10,
        CompletionMatchJobPriority = 0
    };
    Job(int id, Priority priority, unsigned flags = None, QObject *parent = 0);
    ~Job();
    void setPathFilters(const QList<ByteArray> &filter, bool filterSystemIncludes);
    QList<ByteArray> pathFilters() const;
    int id() const { return mId; }
    void write(const ByteArray &out);
    void writeRaw(const ByteArray &out);
    unsigned flags() const { return mFlags; }
    bool filter(const ByteArray &val) const;
    virtual void run();
    virtual void execute() {}
    int priority() const { return mPriority; }
signals:
#if !defined(Q_MOC_RUN)
private:
#endif
    void complete(int id);
    void output(int id, const ByteArray &out);
private:
    const int mId;
    const Priority mPriority;
    const int mFlags;
    QList<ByteArray> mPathFilters;
    bool mFilterSystemIncludes;
};

#endif
