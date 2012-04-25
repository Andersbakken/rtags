#include "Job.h"
#include "Rdm.h"

Job::Job(int id, unsigned flags, QObject *parent)
    : QObject(parent), mId(id), mFlags(flags), mFilterSystemIncludes(false)
{
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

void Job::finish()
{
    emit complete(id());
}

bool Job::filter(const QByteArray &val) const
{
    if (mPathFilters.isEmpty() || (!mFilterSystemIncludes && Rdm::isSystem(val.constData()))) {
        return true;
    }
    QList<QByteArray>::const_iterator it = qUpperBound(mPathFilters, val);
    if (it != mPathFilters.end()) {
        const int cmp = strncmp(val.constData(), (*it).constData(), (*it).size());
        if (cmp == 0) {
            return true;
        } else if (cmp < 0 && it != mPathFilters.begin() && val.startsWith(*(it - 1))) {
            return true;
        }
    } else if (val.startsWith(*(it - 1))) {
        return true;
    }
    return false;
}
