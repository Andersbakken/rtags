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

#ifndef CursorInfoJob_h
#define CursorInfoJob_h

#include <rct/String.h>
#include <rct/List.h>
#include "RTagsClang.h"
#include "Job.h"
#include "Location.h"

class QueryMessage;
class CursorInfoJob : public Job
{
public:
    CursorInfoJob(const Location &loc, const QueryMessage &query, const std::shared_ptr<Project> &proj);
protected:
    virtual void execute();
private:
    const Location location;
};

#endif
