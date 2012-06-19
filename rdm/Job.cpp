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


void Job::setPathFilters(const QList<ByteArray> &filter, bool filterSystemIncludes)
{
    mPathFilters = filter;
    mFilterSystemIncludes = filterSystemIncludes;
}

QList<ByteArray> Job::pathFilters() const
{
    return mPathFilters;
}

void Job::write(const ByteArray &out)
{
    if (mFlags & WriteUnfiltered || mPathFilters.isEmpty() || filter(out)) {
        if (mFlags & QuoteOutput) {
            ByteArray o((out.size() * 2) + 2, '"');
            char *ch = o.data() + 1;
            int l = 2;
            for (int i=0; i<out.size(); ++i) {
                const char c = out.at(i);
                if (c == '"') {
                    *ch = '\\';
                    ch += 2;
                    l += 2;
                } else {
                    ++l;
                    *ch++ = c;
                }
            }
            o.truncate(l);
            emit output(id(), o);
        } else {
            emit output(id(), out);
        }
    }
}

bool Job::filter(const ByteArray &val) const
{
    if (mPathFilters.isEmpty() || (!mFilterSystemIncludes && Path::isSystem(val.constData()))) {
        return true;
    }
    return Rdm::startsWith(mPathFilters, val);
}
void Job::run()
{
    execute();
    emit complete(id());
}
void Job::writeRaw(const ByteArray &out)
{
    emit output(id(), out);
}
