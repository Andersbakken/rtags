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

#include "FindSymbolsJob.h"
#include "Server.h"
#include <rct/Log.h>
#include "RTagsClang.h"
#include "Project.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? QueryJob::QuoteOutput|QueryJob::QuietJob : QueryJob::None|QueryJob::QuietJob;
}

FindSymbolsJob::FindSymbolsJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, ::jobFlags(query->flags()), proj), string(query->query())
{
}

int FindSymbolsJob::execute()
{
    int ret = 2;
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
            ret = count ? 0 : 1;
            for (int i=0; i<count; ++i) {
                write(sorted.at(i).location, writeFlags);
            }
        }
    }
    return ret;
}
