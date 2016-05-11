/* This file is part of RTags (http://rtags.net).

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

#ifndef TokenssJob_h
#define TokenssJob_h

#include "QueryJob.h"
#include "rct/String.h"

class QueryMessage;
class TokensJob : public QueryJob
{
public:
    TokensJob(const std::shared_ptr<QueryMessage> &query,
              uint32_t fileId, uint32_t from, uint32_t to,
              const std::shared_ptr<Project> &project);
protected:
    virtual int execute() override;
private:
    const uint32_t mFileId, mFrom, mTo;
};

#endif
