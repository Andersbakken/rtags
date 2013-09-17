/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "IndexerJob.h"
#include <rct/StopWatch.h>
#include "Project.h"

// #define TIMINGS_ENABLED
#ifdef TIMINGS_ENABLED
static std::mutex mutex;
static Map<const char*, uint64_t> times;
static void addTiming(const char *name, uint64_t usec)
{
    std::lock_guard<std::mutex> lock(mutex);
    times[name] += usec;
}

struct TimingNode
{
    const char *key;
    uint64_t usecs;
    bool operator<(const TimingNode &other) const { return usecs > other.usecs; }
};

static void dumpTimings()
{
    std::lock_guard<std::mutex> lock(mutex);
    List<TimingNode> nodes;
    uint64_t tot = 0;
    for (Map<const char*, uint64_t>::const_iterator it = times.begin(); it != times.end(); ++it) {
        if (!it->first) {
            TimingNode node = { "Total", it->second };
            nodes.append(node);
            tot = it->second;
        } else {
            TimingNode node = { it->first, it->second };
            nodes.append(node);
        }
    }
    if (tot) {
        std::sort(nodes.begin(), nodes.end());
        error("Timings:\n---------------------------");
        for (int i=0; i<nodes.size(); ++i) {
            error("%s: %llums (%.1f%%)", nodes.at(i).key, static_cast<unsigned long long>(nodes.at(i).usecs) / 1000,
                  (static_cast<double>(nodes.at(i).usecs) / static_cast<double>(tot)) * 100.0);
        }
    }
}
class Timing
{
public:
    Timing(const char *n) : name(n), watch(StopWatch::Microsecond) {}
    ~Timing() { addTiming(name, watch.elapsed()); }
    const char *name;
    StopWatch watch;
};
#define TIMING() Timing timing(__FUNCTION__)
#define NAMED_TIMING(name) addTiming(name, timing.watch.elapsed())
#else
#define TIMING() do {} while (0)
#define NAMED_TIMING(name) do {} while (0)
#endif

IndexerJob::IndexerJob(const std::shared_ptr<Project> &project, Type type, const SourceInformation &sourceInformation)
    : Job(0, project), mType(type), mSourceInformation(sourceInformation), mTimer(StopWatch::Microsecond),
      mParseTime(0), mStarted(false)
{}

IndexerJob::IndexerJob(const QueryMessage &msg, const std::shared_ptr<Project> &project,
                       const SourceInformation &sourceInformation)
    : Job(msg, WriteUnfiltered|WriteBuffered|QuietJob, project), mType(Dump), mSourceInformation(sourceInformation),
      mTimer(StopWatch::Microsecond), mParseTime(0), mStarted(false)
{
}

IndexerJob::~IndexerJob()
{
#ifdef TIMINGS_ENABLED
    addTiming(0, mTimer.elapsed()); // in ms
    dumpTimings();
#endif
}

Location IndexerJob::createLocation(const Path &file, uint32_t offset, bool *blocked)
{
    uint32_t &fileId = mFileIds[file];
    if (!fileId) {
        const Path resolved = file.resolved();
        fileId = mFileIds[resolved] = Location::insertFile(resolved);
    }
    return createLocation(fileId, offset, blocked);
}


Location IndexerJob::createLocation(uint32_t fileId, uint32_t offset, bool *blocked)
{
    TIMING();
    if (blocked)
        *blocked = false;
    if (!fileId)
        return Location();
    if (blocked) {
        if (mVisitedFiles.contains(fileId)) {
            *blocked = false;
        } else if (mBlockedFiles.contains(fileId)) {
            *blocked = true;
        } else {
            std::shared_ptr<Project> p = project();
            if (!p) {
                return Location();
            } else if (p->visitFile(fileId)) {
                if (blocked)
                    *blocked = false;
                mVisitedFiles.insert(fileId);
                mData->errors[fileId] = 0;
            } else {
                mBlockedFiles.insert(fileId);
                if (blocked)
                    *blocked = true;
                return Location();
            }
        }
    }
    return Location(fileId, offset);
}

bool IndexerJob::abortIfStarted()
{
    std::lock_guard<std::mutex> lock(mutex());
    if (mStarted)
        aborted() = true;
    return aborted();
}

void IndexerJob::execute()
{
    {
        std::lock_guard<std::mutex> lock(mutex());
        mStarted = true;
    }
    mTimer.restart();
    mData = createIndexData();
    assert(mData);

    index();
    IndexerJob::SharedPtr that = std::static_pointer_cast<IndexerJob>(shared_from_this());
    mFinished(that);
}
