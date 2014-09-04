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

#include "ScanThread.h"
#include "Server.h"
#include "Filter.h"
#include "Project.h"

ScanThread::ScanThread(const Path &path)
    : Thread(), mPath(path), mFilters(Server::instance()->options().excludeFilters)
{
}

struct UserData {
    Set<Path> paths;
    const List<String> &filters;
};

static Path::VisitResult visit(const Path &path, void *userData)
{
    UserData *u = reinterpret_cast<UserData*>(userData);
    const Filter::Result result = Filter::filter(path, u->filters);
    switch (result) {
    case Filter::Filtered:
        return Path::Continue;
    case Filter::Directory:
        if (Path::exists(path + "/.rtags-ignore"))
            return Path::Continue;
        return Path::Recurse;
    case Filter::File:
    case Filter::Source:
        u->paths.insert(path);
        break;
    }
    return Path::Continue;
}

Set<Path> ScanThread::paths(const Path &path, const List<String> &filters)
{
    UserData userData = { Set<Path>(), filters };
    path.visit(&::visit, &userData);
    return userData.paths;
}

void ScanThread::run()
{
    mFinished(paths(mPath, mFilters));
}


