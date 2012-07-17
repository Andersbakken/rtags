#include "Job.h"
#include "RTags.h"
#include "EventLoop.h"
#include "Server.h"
#include "CursorInfo.h"

// static int count = 0;
// static int active = 0;
Job::Job(unsigned jobFlags)
    : mJobFlags(jobFlags), mQueryFlags(0)
{
    setAutoDelete(false);
}

Job::Job(const QueryMessage &query, unsigned jobFlags)
    : mId(-1), mJobFlags(jobFlags), mQueryFlags(query.flags())
{
    setPathFilters(query.pathFilters());
    setAutoDelete(false);
}


Job::~Job()
{
}

void Job::setPathFilters(const List<ByteArray> &filter)
{
    mPathFilters = filter;
}

List<ByteArray> Job::pathFilters() const
{
    return mPathFilters;
}

void Job::write(const ByteArray &out)
{
    if (mJobFlags & WriteUnfiltered || mPathFilters.isEmpty() || filter(out)) {
        if (mJobFlags & QuoteOutput) {
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
            writeRaw(o);
        } else {
            writeRaw(out);
        }
    }
}

bool Job::filter(const ByteArray &val) const
{
    if (mPathFilters.isEmpty() || ((!mQueryFlags & QueryMessage::FilterSystemIncludes) && Path::isSystem(val.constData()))) {
        return true;
    }
    return RTags::startsWith(mPathFilters, val);
}
void Job::run()
{
    execute();
    if (mId != -1)
        EventLoop::instance()->postEvent(Server::instance(), new JobCompleteEvent(this));
}
void Job::writeRaw(const ByteArray &out)
{
    if (mJobFlags & OutputSignalEnabled) {
        output()(out);
    } else {
        EventLoop::instance()->postEvent(Server::instance(), new JobOutputEvent(this, out));
    }
}

void Job::write(const Location &location, const CursorInfo &ci)
{
    if (ci.symbolLength) {
        char buf[1024];
        const CXStringScope kind(clang_getCursorKindSpelling(ci.kind));
        const int w = snprintf(buf, sizeof(buf), "%s symbolName: %s kind: %s isDefinition: %s symbolLength: %d %s%s%s",
                               location.key().constData(), ci.symbolName.constData(),
                               clang_getCString(kind.string), ci.isDefinition ? "true" : "false",
                               ci.symbolLength, ci.target.isValid() ? "target: " : "", ci.target.isValid() ? ci.target.key().constData() : "",
                               (ci.references.isEmpty() && ci.additionalReferences.isEmpty() ? "" : " references:"));
        write(ByteArray(buf, w));
        for (Set<Location>::const_iterator rit = ci.references.begin(); rit != ci.references.end(); ++rit) {
            const Location &l = *rit;
            snprintf(buf, sizeof(buf), "    %s", l.key().constData());
            write(buf);
        }
        for (Set<Location>::const_iterator rit = ci.additionalReferences.begin(); rit != ci.additionalReferences.end(); ++rit) {
            const Location &l = *rit;
            snprintf(buf, sizeof(buf), "    %s (additional)", l.key().constData());
            write(buf);
        }
    }

}
unsigned Job::keyFlags() const
{
    return QueryMessage::keyFlags(mQueryFlags);
}
