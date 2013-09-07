#include "FindSymbolsJob.h"
#include "Server.h"
#include <rct/Log.h>
#include "RTagsClang.h"
#include "Project.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::QuoteOutput|Job::QuietJob : Job::None|Job::QuietJob;
}

FindSymbolsJob::FindSymbolsJob(const QueryMessage &query, const std::shared_ptr<Project> &proj)
    : Job(query, ::jobFlags(query.flags()), proj), string(query.query())
{
}

void FindSymbolsJob::execute()
{
    if (std::shared_ptr<Project> proj = project()) {
        const uint32_t filter = fileFilter();
        const Set<Location> locations = proj->locations(string, filter);
        if (!locations.isEmpty()) {
            unsigned int sortFlags = Project::Sort_None;
            if (queryFlags() & QueryMessage::DeclarationOnly)
                sortFlags |= Project::Sort_DeclarationOnly;
            if (queryFlags() & QueryMessage::ReverseSort)
                sortFlags |= Project::Sort_Reverse;

            const List<RTags::SortedCursor> sorted = proj->sort(locations, sortFlags);
            const unsigned int writeFlags = filter ? Unfiltered : NoWriteFlags;
            const int count = sorted.size();
            for (int i=0; i<count; ++i) {
                write(sorted.at(i).location, writeFlags);
            }
        }
    }
}
