#include "Job.h"
#include "RTags.h"
#include <rct/EventLoop.h>
#include "Server.h"
#include "CursorInfo.h"
#include <rct/RegExp.h>
#include "QueryMessage.h"
#include "Project.h"

// static int count = 0;
// static int active = 0;

Job::Job(const QueryMessage &query, unsigned jobFlags, const shared_ptr<Project> &proj)
    : mAborted(false), mId(-1), mMinOffset(query.minOffset()),
      mMaxOffset(query.maxOffset()), mJobFlags(jobFlags), mQueryFlags(query.flags()), mProject(proj),
      mPathFilters(0), mPathFiltersRegExp(0), mMax(query.max()), mConnection(0),
      mContext(query.context())
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

    if (!(mJobFlags & QuietJob))
        error("=> %s", out.constData());

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
            EventLoop::eventLoop()->callLaterMove(std::bind(&Server::onJobOutput, Server::instance(), std::placeholders::_1),
                                                  JobOutput(shared_from_this(), mBuffer, false));
            mBuffer.clear();
            mBuffer.reserve(BufSize);
        }
        if (!mBuffer.isEmpty())
            mBuffer.append('\n');
        mBuffer.append(out);
    } else {
        EventLoop::eventLoop()->callLaterMove(std::bind(&Server::onJobOutput, Server::instance(), std::placeholders::_1),
                                              JobOutput(shared_from_this(), out, false));
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
    String out = location.key(keyFlags());
    if (queryFlags() & QueryMessage::ContainingFunction) {
        const SymbolMap &symbols = project()->symbols();
        SymbolMap::const_iterator it = symbols.find(location);
        if (it == symbols.end()) {
            error() << "Somehow can't find" << location << "in symbols";
        } else {
            const uint32_t fileId = location.fileId();
            const int offset = location.offset();
            while (true) {
                --it;
                if (it->first.fileId() != fileId)
                    break;
                if (it->second.isDefinition() && RTags::isContainer(it->second.kind) && offset >= it->second.start && offset <= it->second.end) {
                    out += "\tfunction: " + it->second.symbolName;
                    break;
                } else if (it == symbols.begin()) {
                    break;
                }
            }
        }
    }
    return write(out);
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

bool Job::filter(const String &value) const
{
    if (!mPathFilters && !mPathFiltersRegExp && !(mQueryFlags & QueryMessage::FilterSystemIncludes))
        return true;

    const char *val = value.constData();
    while (*val && isspace(*val))
        ++val;

    if (mQueryFlags & QueryMessage::FilterSystemIncludes && Path::isSystem(val))
        return false;

    if (!mPathFilters && !mPathFiltersRegExp)
        return true;

    assert(!mPathFilters != !mPathFiltersRegExp);
    String copy;
    const String &ref = (val != value.constData() ? copy : value);
    if (val != value.constData())
        copy = val;
    if (mPathFilters)
        return RTags::startsWith(*mPathFilters, ref);

    assert(mPathFiltersRegExp);

    const int count = mPathFiltersRegExp->size();
    for (int i=0; i<count; ++i) {
        if (mPathFiltersRegExp->at(i).indexIn(ref) != -1)
            return true;
    }
    return false;
}


unsigned Job::keyFlags() const
{
    return QueryMessage::keyFlags(mQueryFlags);
}

void Job::run()
{
    execute();
    if (mId != -1) {
        EventLoop::eventLoop()->callLaterMove(std::bind(&Server::onJobOutput, Server::instance(), std::placeholders::_1),
                                              JobOutput(shared_from_this(), mBuffer, true));
    }
}

void Job::run(Connection *connection)
{
    assert(connection);
    mConnection = connection;
    execute();
    mConnection = 0;
}
