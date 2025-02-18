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

#include "ScanThread.h"

#include <map>

#include "Filter.h"
#include "Server.h"

ScanThread::ScanThread(const Path &path)
    : Thread(), mPath(path), mFilters(Server::instance()->options().excludeFilters)
{
}

Set<Path> ScanThread::paths(const Path &path, const List<String> &filters)
{
    Set<Path> paths;
    path.visit([&paths, filters](const Path &p) {
            const Filter::Result result = Filter::filter(p, filters);
            switch (result) {
            case Filter::Filtered:
                return Path::Continue;
            case Filter::Directory:
                if (Path::exists(p + "/.rtags-ignore"))
                    return Path::Continue;
                return Path::Recurse;
            case Filter::File:
            case Filter::Source:
                paths.insert(p);
                break;
            }
            return Path::Continue;
        });
    return paths;
}

void ScanThread::run()
{
    mFinished(paths(mPath, mFilters));
}
