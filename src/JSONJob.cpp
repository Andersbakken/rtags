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

#include "JSONJob.h"
#include "SymbolInfo.h"
#include "Project.h"
#include "RTags.h"
#include "Server.h"

JSONJob::JSONJob(const std::shared_ptr<QueryMessage> &q, const std::shared_ptr<Project> &project)
    : Job(q, WriteUnfiltered|QuietJob, project), match(q.match())
{
    assert(project.get());
}

static String toJSON(const Location &loc, uint32_t fileId, int length, int srcRootLength)
{
    if (loc.fileId() == fileId) {
        return String::format<64>("{\"offset\":%d,\"length\":%d}", loc.offset(), length);
    } else {
        return String::format<64>("{\"file\":\"%s\",\"offset\":%d,\"length\":%d}",
                                     Location::path(loc.fileId()).constData() + srcRootLength, loc.offset(), length);
    }
}

void JSONJob::execute()
{
    std::shared_ptr<Project> proj = project();
    const Path root = proj->path();
    const Dependencies deps = proj->dependencies();
    // error() << deps.keys();
    assert(proj);
    const SymbolMap &map = proj->symbols();
    write("{");
    bool firstObject = true;
    for (Dependencies::const_iterator it = deps.begin(); it != deps.end(); ++it) {
        const Path path = Location::path(it->first);
        if (path.startsWith(root) && (match.isEmpty() || match.match(path))) {
            const Location loc(it->first, 0);
            const int srcRootLength = project()->path().size();
            if (firstObject) {
                firstObject = false;
            } else {
                write(",");
            }
            write<64>("\"%s\":[", path.constData() + srcRootLength);
            bool firstSymbol = true;
            SymbolMap::const_iterator sit = map.lower_bound(loc);
            while (sit != map.end() && sit->first.fileId() == it->first) {
                Location targetLocation;
                SymbolInfo target = sit->second.bestTarget(map, 0, &targetLocation);
                const String type = sit->second.kindSpelling();
                if (firstSymbol) {
                    firstSymbol = false;
                } else {
                    write(",");
                }
                if (!targetLocation.isNull()) {
                    write<256>("{\"location\":%s,\"type\":\"%s\",\"target\":%s}",
                               toJSON(sit->first, it->first, sit->second.symbolLength, srcRootLength).constData(), type.constData(),
                               toJSON(targetLocation, it->first, target.symbolLength, srcRootLength).constData());
                } else {
                    write<256>("{\"location\":%s,\"type\":\"%s\"}",
                               toJSON(sit->first, it->first, sit->second.symbolLength, srcRootLength).constData(), type.constData());
                }

                ++sit;
            }
            write("]");
        }
    }
    write("}");
}

