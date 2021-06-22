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

#ifndef ScanThread_h
#define ScanThread_h

#include <functional>

#include "rct/Path.h"
#include "rct/SignalSlot.h"
#include "rct/Thread.h"
#include <vector>
#include <set>
#include "rct/String.h"

class Project;

class ScanThread : public Thread
{
public:
    ScanThread(const Path &path);
    virtual void run() override;
    Signal<std::function<void(std::set<Path>)> > &finished() { return mFinished; }
    static std::set<Path> paths(const Path &path, const std::vector<String> &filters = std::vector<String>());
private:
    Path mPath;
    const std::vector<String> &mFilters;
    Signal<std::function<void(std::set<Path>)> > mFinished;
};

#endif
