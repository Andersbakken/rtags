#include "Job.h"
#include "RTags.h"
#include "EventLoop.h"
#include "Server.h"
#include "CursorInfo.h"
#include "RegExp.h"
#include "QueryMessage.h"

// static int count = 0;
// static int active = 0;

Job::Job(const QueryMessage &query, unsigned jobFlags, const shared_ptr<Project> &proj)
    : mId(-1), mJobFlags(jobFlags), mQueryFlags(query.flags()), mProject(proj), mPathFilters(0),
      mPathFiltersRegExp(0), mMax(query.max())
{
    const List<ByteArray> &pathFilters = query.pathFilters();
    if (!pathFilters.isEmpty()) {
        if (mQueryFlags & QueryMessage::MatchRegexp) {
            mPathFiltersRegExp = new List<RegExp>();
            const int size = pathFilters.size();
            mPathFiltersRegExp->reserve(size);
            for (int i=0; i<size; ++i) {
                mPathFiltersRegExp->append(pathFilters.at(i));
            }
        } else {
            mPathFilters = new List<ByteArray>(pathFilters);
        }
    }
}

Job::Job(unsigned jobFlags, const shared_ptr<Project> &proj)
    : mId(-1), mJobFlags(jobFlags), mQueryFlags(0), mProject(proj), mPathFilters(0), mPathFiltersRegExp(0), mMax(-1)
{
}

Job::~Job()
{
    delete mPathFilters;
    delete mPathFiltersRegExp;
}

bool Job::write(const ByteArray &out, unsigned flags)
{
    if (mJobFlags & WriteUnfiltered || filter(out)) {
        if ((mJobFlags & QuoteOutput) && !(flags & DontQuote)) {
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
            return writeRaw(o, flags);
        } else {
            return writeRaw(out, flags);
        }
    }
    return true;
}

bool Job::writeRaw(const ByteArray &out, unsigned flags)
{
    if (!(flags & IgnoreMax)) {
        switch (mMax) {
        case 0:
        case -1:
            break;
        default:
            --mMax;
            break;
        }
    }

    if (mJobFlags & WriteBuffered) {
        enum { BufSize = 16384 };
        if (mBuffer.size() + out.size() + 1 > BufSize) {
            EventLoop::instance()->postEvent(Server::instance(), new JobOutputEvent(shared_from_this(), mBuffer, false));
            mBuffer.clear();
            mBuffer.reserve(BufSize);
        }
        if (!mBuffer.isEmpty())
            mBuffer.append('\n');
        mBuffer.append(out);
    } else {
        EventLoop::instance()->postEvent(Server::instance(), new JobOutputEvent(shared_from_this(), out, false));
    }
    return true;
}

bool Job::write(const Location &location, unsigned flags)
{
    if (location.isNull())
        return false;
    const unsigned kf = keyFlags();
    if (!write<1024>(flags, "%s", location.key(kf).constData()))
        return false;
    return true;
}

bool Job::write(const CursorInfo &ci, unsigned flags)
{
    if (ci.isNull())
        return false;
    const unsigned kf = keyFlags();
    if (!write<1024>(flags, "%s", ci.toString(kf).constData()))
        return false;
    return true;
}


unsigned Job::keyFlags() const
{
    return QueryMessage::keyFlags(mQueryFlags);
}
void Job::run()
{
    execute();
    if (mId != -1)
        EventLoop::instance()->postEvent(Server::instance(), new JobOutputEvent(shared_from_this(), mBuffer, true));
}
