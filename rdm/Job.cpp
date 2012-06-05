#include "Job.h"
#include "Rdm.h"

// static int count = 0;
// static int active = 0;
Job::Job(int id, Priority p, unsigned flags, QObject *parent)
    : QObject(parent), mId(id), mPriority(p), mFlags(flags), mFilterSystemIncludes(false)
{
    // qDebug() << metaObject()->className() << "born" << ++count << ++active;
    setAutoDelete(false);
}

Job::~Job()
{
    // qDebug() << metaObject()->className() << "died" << count << --active;
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
    if (mFlags & WriteUnfiltered || mPathFilters.isEmpty() || filter(out)) {
        if (mFlags & QuoteOutput) {
            QByteArray o(out.size() + 2, '"');
            memcpy(o.data() + 1, out.constData(), out.size());
            emit output(id(), o);
        } else {
            emit output(id(), out);
        }
    }
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
void Job::writeRaw(const QByteArray &out)
{
    emit output(id(), out);
}
