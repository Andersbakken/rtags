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

#include "IncludeFileJob.h"
#include "RTags.h"
#include "Server.h"
#include "Project.h"

IncludeFileJob::IncludeFileJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &project)
    : QueryJob(query, project)
{
    const uint32_t fileId = Location::fileId(query->currentFile());
    mSource = project->sources(fileId).value(query->buildIndex());
    if (mSource.isNull()) {
        for (const uint32_t dep : project->dependencies(fileId, Project::DependsOnArg)) {
            mSource = project->sources(dep).value(query->buildIndex());
            if (!mSource.isNull())
                break;
        }
    }
    mSymbol = query->query();
}

int IncludeFileJob::execute()
{
    if (mSource.isNull())
        return 1;
    const Path directory = mSource.sourceFile().parentDir();
    const bool fromHeader = queryMessage()->currentFile().isHeader();
    Set<Location> last;
    int matches = 0;
    auto process = [&directory, &fromHeader, this](const Set<Location> &locations) {
        for (const Location &loc : locations) {
            const Path path = loc.path();
            if (!path.isHeader())
                continue;
            const Symbol sym = project()->findSymbol(loc);
            if (sym.isDefinition() || !sym.isClass()) {
                if (!fromHeader && path.startsWith(directory)) {
                    write<256>("#include \"%s\"", path.mid(directory.size()).constData());
                } else {
                    for (const Source::Include &inc : mSource.includePaths) {
                        const Path p = inc.path.ensureTrailingSlash();
                        if (path.startsWith(p)) {
                            write<256>("#include <%s>", path.mid(p.size()).constData());
                        }
                    }
                }
            }
        }
    };
    project()->findSymbols(mSymbol, [&](Project::SymbolMatchType type, const String &, const Set<Location> &locations) {
            ++matches;
            if (type != Project::StartsWith) {
                process(locations);
            } else if (matches == 1) {
                last = locations;
            }
        }, queryFlags());
    if (matches == 1 && !last.isEmpty()) {
        process(last);
    }

    return 0;
}
