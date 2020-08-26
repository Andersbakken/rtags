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

#ifndef SymbolInfoJob_h
#define SymbolInfoJob_h

#include <memory>

#include "Location.h"
#include "QueryJob.h"

class QueryMessage;
class Project;
class String;
template <typename T> class Set;

class SymbolInfoJob : public QueryJob
{
public:
    SymbolInfoJob(Location s, Location e, Set<String> &&pieceFilters,
                  const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj);
protected:
    virtual int execute() override;
private:
    const Location start, end;
};

#endif
