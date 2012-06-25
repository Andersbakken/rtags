#include "Job.h"
#include "Rdm.h"
#include "EventLoop.h"
#include "Server.h"

// static int count = 0;
// static int active = 0;
Job::Job(int id, Priority p, unsigned flags)
    : mId(id), mPriority(p), mFlags(flags), mFilterSystemIncludes(false)
{
    // error() << metaObject()->className() << "born" << ++count << ++active;
    setAutoDelete(false);
}

Job::~Job()
{
    // error() << metaObject()->className() << "died" << count << --active;
}


void Job::setPathFilters(const List<ByteArray> &filter, bool filterSystemIncludes)
{
    mPathFilters = filter;
    mFilterSystemIncludes = filterSystemIncludes;
}

List<ByteArray> Job::pathFilters() const
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
            EventLoop::instance()->postEvent(Server::instance(), new JobOutputEvent(this, o));
        } else {
            EventLoop::instance()->postEvent(Server::instance(), new JobOutputEvent(this, out));
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
    EventLoop::instance()->postEvent(Server::instance(), new JobCompleteEvent(this));
}
void Job::writeRaw(const ByteArray &out)
{
    EventLoop::instance()->postEvent(Server::instance(), new JobOutputEvent(this, out));
}
