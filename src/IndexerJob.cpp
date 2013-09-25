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

IndexerJob::IndexerJob(const std::shared_ptr<Project> &project, Type type, const SourceInformation &sourceInformation)
    : Job(0, project), mType(type), mLogFile(0), mSourceInformation(sourceInformation),
      mParseTime(0), mStarted(false)
{}

IndexerJob::IndexerJob(const QueryMessage &msg, const std::shared_ptr<Project> &project,
                       const SourceInformation &sourceInformation)
    : Job(msg, WriteUnfiltered|WriteBuffered|QuietJob, project), mType(Dump), mLogFile(0),
      mSourceInformation(sourceInformation), mParseTime(0), mStarted(false)
{
}

IndexerJob::~IndexerJob()
{
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
                if (mLogFile)
                    fprintf(mLogFile, "WON %s\n", Location::path(fileId).constData());
                mVisitedFiles.insert(fileId);
                mData->errors[fileId] = 0;
            } else {
                if (mLogFile)
                    fprintf(mLogFile, "LOST %s\n", Location::path(fileId).constData());
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
