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

#include "ScanJob.h"
#include "Server.h"
#include "Filter.h"
#include "Project.h"

ScanJob::ScanJob(const Path &path)
    : mPath(path), mFilters(Server::instance()->options().excludeFilters)
{
    if (!mPath.endsWith('/'))
        mPath.append('/');
}

void ScanJob::run()
{
    mPath.visit(&ScanJob::visit, this);
    mFinished(mPaths);
}

Path::VisitResult ScanJob::visit(const Path &path, void *userData)
{
    ScanJob *recurseJob = reinterpret_cast<ScanJob*>(userData);
    const Filter::Result result = Filter::filter(path, recurseJob->mFilters);
    switch (result) {
    case Filter::Filtered:
        return Path::Continue;
    case Filter::Directory:
        if (Path::exists(path + "/.rtags-ignore"))
            return Path::Continue;
        return Path::Recurse;
    case Filter::File:
    case Filter::Source:
        recurseJob->mPaths.insert(path);
        break;
    }
    return Path::Continue;
}
