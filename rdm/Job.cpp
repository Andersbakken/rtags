#include "Job.h"
#include "Rdm.h"

Job::Job(int id, unsigned flags, QObject *parent)
    : QObject(parent), mId(id), mFlags(flags), mFilterSystemIncludes(false)
{
    setAutoDelete(false);
}

void Job::setPathFilters(const QList<QByteArray> &filter, bool filterSystemIncludes)
{
    mPathFilters = filter;
    mFilterSystemIncludes = filterSystemIncludes;
}

QList<QByteArray> Job::pathFilters() const
{
    return mPathFilters;
}

void Job::write(const QByteArray &out)
{
    if (mFlags & WriteUnfiltered || mPathFilters.isEmpty() || filter(out))
        emit output(id(), out);
}

bool Job::filter(const QByteArray &val) const
{
    if (mPathFilters.isEmpty() || (!mFilterSystemIncludes && Rdm::isSystem(val.constData()))) {
        return true;
    }
    return Rdm::startsWith(mPathFilters, val);
}
void Job::run()
{
    execute();
    emit complete(id());
}
