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

#ifndef RTagsPlugin_h
#define RTagsPlugin_h

#include <rct/Path.h>
#include <rct/List.h>
#include "IndexerJob.h"

class Project;
class QueryMessage;
class SourceInformation;
class Connection;
class RTagsPlugin
{
public:
    virtual ~RTagsPlugin() {}
    virtual std::shared_ptr<IndexerJob> createJob(IndexType type,
                                                  const std::shared_ptr<Project> &project,
                                                  const SourceInformation &sourceInformation) = 0;
    virtual std::shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                                  const std::shared_ptr<Project> &project,
                                                  const SourceInformation &sourceInformation,
                                                  Connection *conn) = 0;
    // add functions for completions etc
};

#endif
