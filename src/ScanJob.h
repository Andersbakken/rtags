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

#ifndef ScanJob_h
#define ScanJob_h

#include <rct/ThreadPool.h>
#include <rct/Path.h>
#include <rct/SignalSlot.h>
class Project;
class ScanJob : public ThreadPool::Job
{
public:
    ScanJob(const Path &path);
    virtual void run();
    Signal<std::function<void(Set<Path>)> > &finished() { return mFinished; }
private:
    static Path::VisitResult visit(const Path &path, void *userData);
    Path mPath;
    const List<String> &mFilters;
    Set<Path> mPaths;
    Signal<std::function<void(Set<Path>)> > mFinished;
};

#endif
