#include "Job.h"
#include "RTags.h"
#include <rct/EventLoop.h>
#include "Server.h"
#include "CursorInfo.h"
#include <rct/RegExp.h>
#include "QueryMessage.h"

// static int count = 0;
// static int active = 0;

Job::Job(const QueryMessage &query, unsigned jobFlags, const shared_ptr<Project> &proj)
    : mAborted(false), mId(-1), mMinOffset(query.minOffset()),
      mMaxOffset(query.maxOffset()), mJobFlags(jobFlags), mQueryFlags(query.flags()), mProject(proj),
      mPathFilters(0), mPathFiltersRegExp(0), mMax(query.max()), mConnection(0)
{
    const List<String> &pathFilters = query.pathFilters();
    if (!pathFilters.isEmpty()) {
        if (mQueryFlags & QueryMessage::MatchRegexp) {
            mPathFiltersRegExp = new List<RegExp>();
            const int size = pathFilters.size();
            mPathFiltersRegExp->reserve(size);
            for (int i=0; i<size; ++i) {
                mPathFiltersRegExp->append(pathFilters.at(i));
            }
        } else {
            mPathFilters = new List<String>(pathFilters);
        }
    }
}

Job::Job(unsigned jobFlags, const shared_ptr<Project> &proj)
    : mAborted(false), mId(-1), mMinOffset(-1), mMaxOffset(-1), mJobFlags(jobFlags), mQueryFlags(0), mProject(proj), mPathFilters(0),
      mPathFiltersRegExp(0), mMax(-1), mConnection(0)
{
}

Job::~Job()
{
    delete mPathFilters;
    delete mPathFiltersRegExp;
}

bool Job::write(const String &out, unsigned flags)
{
    if (mJobFlags & WriteUnfiltered || filter(out)) {
        if ((mJobFlags & QuoteOutput) && !(flags & DontQuote)) {
            String o((out.size() * 2) + 2, '"');
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

bool Job::writeRaw(const String &out, unsigned flags)
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

    if (mConnection) {
        if (!mConnection->write(out)) {
            abort();
            return false;
        }
        return true;
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
    if (mMinOffset != -1) {
        assert(mMaxOffset != -1);
        const int offset = location.offset();
        if (offset < mMinOffset || offset > mMaxOffset) {
            return false;
        }
    }
    if (!write(location.key(keyFlags()).constData()))
        return false;
    return true;
}

bool Job::write(const CursorInfo &ci, unsigned ciflags)
{
    if (ci.isNull())
        return false;
    const unsigned kf = keyFlags();
    if (!write(ci.toString(ciflags, kf).constData()))
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

void Job::run(Connection *connection)
{
    assert(connection);
    mConnection = connection;
    execute();
    mConnection = 0;
}
