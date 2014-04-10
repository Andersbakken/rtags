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

#ifndef PreprocessorJob_h
#define PreprocessorJob_h

#include <rct/ThreadPool.h>
#include <rct/SignalSlot.h>
#include "CompileMessage.h"
#include "IndexerJob.h"
#include "Source.h"

class Project;
class PreprocessJob : public ThreadPool::Job
{
public:
    PreprocessJob(Source &&source, const std::shared_ptr<Project> &project, uint32_t indexerJobFlags);
    const Source &source() const { return mSource; }
protected:
    virtual void run();
private:
    Source mSource;
    std::shared_ptr<Project> mProject;
    const uint32_t mFlags;
    bool mCompression;
};

#endif
