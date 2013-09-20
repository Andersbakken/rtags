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

#ifndef ValidateDBJob_h
#define ValidateDBJob_h

#include "Job.h"
#include <rct/Set.h>
#include "Location.h"
#include <rct/SignalSlot.h>

class ValidateDBJob : public Job
{
public:
    ValidateDBJob(const std::shared_ptr<Project> &proj, const Set<Location> &prev);
    Signal<std::function<void(const Set<Location> &)> > &errors() { return mErrors; }
protected:
    virtual void execute();
private:
    const Set<Location> mPrevious;
    Signal<std::function<void(const Set<Location> &)> > mErrors;

};

#endif
