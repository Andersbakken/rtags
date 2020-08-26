/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef ReferencesJob_h
#define ReferencesJob_h

#include <memory>

#include "Location.h"
#include "QueryJob.h"
#include "rct/String.h"
#include "rct/Set.h"

class SymbolInfo;
class Project;
class QueryMessage;

class ReferencesJob : public QueryJob
{
public:
    ReferencesJob(Location location, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &project);
    ReferencesJob(const String &symbolName, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &project);
protected:
    virtual int execute() override;
private:
    Set<Location> mLocations;
    const String mSymbolName;
};

#endif
