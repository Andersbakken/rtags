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
    : mId(-1), mJobFlags(jobFlags), mQueryFlags(query.flags()), mProject(proj), mPathFilters(0), mPathFiltersRegExp(0)
{
    const List<ByteArray> &pathFilters = query.pathFilters();
    if (!pathFilters.isEmpty()) {
        if (mQueryFlags & QueryMessage::PathMatchRegExp) {
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
    : mId(-1), mJobFlags(jobFlags), mQueryFlags(0), mProject(proj), mPathFilters(0), mPathFiltersRegExp(0)
{
}

Job::~Job()
{
    delete mPathFilters;
    delete mPathFiltersRegExp;
}

void Job::write(const ByteArray &out)
{
    if (mJobFlags & WriteUnfiltered || filter(out)) {
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

void Job::run()
{
    execute();
    if (mId != -1)
        EventLoop::instance()->postEvent(Server::instance(), new JobCompleteEvent(mId));
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
        const int w = snprintf(buf, sizeof(buf), "%s symbolName: %s kind: %s %s symbolLength: %d %s%s%s",
                               location.key().constData(), ci.symbolName.constData(),
                               clang_getCString(kind.string),
                               ci.isDefinition ? "Definition" : RTags::isReference(ci.kind) ? "Reference" : "Declaration",
                               ci.symbolLength, ci.target.isValid() ? "target: " : "", ci.target.isValid() ? ci.target.key().constData() : "",
                               ci.references.isEmpty() ? "" : "references:");
        write(ByteArray(buf, w));
        for (Set<Location>::const_iterator rit = ci.references.begin(); rit != ci.references.end(); ++rit) {
            const Location &l = *rit;
            snprintf(buf, sizeof(buf), "    %s", l.key().constData());
            write(buf);
        }
    }

}
unsigned Job::keyFlags() const
{
    return QueryMessage::keyFlags(mQueryFlags);
}

ScopedDB Job::db(Server::DatabaseType type, ReadWriteLock::LockType lockType) const
{
    return Server::instance()->db(type, lockType);
}

ScopedDB Job::db(Project::DatabaseType type, ReadWriteLock::LockType lockType) const
{
    return mProject ? mProject->db(type, lockType) : ScopedDB();
}
