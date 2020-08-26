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

#include "IncludePathJob.h"

#include <stddef.h>
#include <cstdint>
#include <functional>
#include <utility>

#include "Project.h"
#include "RTags.h"
#include "FileMap.h"
#include "QueryMessage.h"
#include "Symbol.h"
#include "clang-c/Index.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Path.h"
#include "rct/String.h"

IncludePathJob::IncludePathJob(const Location loc, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &project)
    : QueryJob(query, project, QuietJob), location(loc)
{
}

int IncludePathJob::execute()
{
    int idx = 0;
    Symbol symbol = project()->findSymbol(location, &idx);
    if (symbol.isNull()) {
        return 1;
    }

    // if you invoke a destructor explicitly there's a typeref on the class
    // name. This finds the destructor instead.
    if ((symbol.kind == CXCursor_TypeRef || symbol.kind == CXCursor_TemplateRef) && idx > 0) {
        auto symbols = project()->openSymbols(location.fileId());
        if (!symbols || !symbols->count())
            return 1;
        const Symbol prev = symbols->valueAt(idx - 1);
        if (prev.kind == CXCursor_MemberRefExpr
            && prev.location.column() == symbol.location.column() - 1
            && prev.location.line() == symbol.location.line()
            && prev.symbolName.contains("~")) {
            symbol = prev;
        }
    }

    auto targets = RTags::sortTargets(project()->findTargets(symbol));

    String allTargetPath;
    int maxDepth = queryMessage()->maxDepth();
    if (maxDepth == -1)
        maxDepth = 8;
    const bool absolute = queryFlags() & QueryMessage::AbsolutePath;
    const std::shared_ptr<Project> proj = project();
    if (!proj)
        return 1;
    const Path projectPath = proj->path();
    for (const auto &target : targets) {
        DependencyNode *depNode = project()->dependencyNode(location.fileId());
        if (depNode) {
            List<uint32_t> paths;
            bool done = false;
            std::function<void(DependencyNode *)> process = [&](DependencyNode *n) {
                if (done || !paths.contains(n->fileId)) {
                    paths.append(n->fileId);
                    if (n->fileId == target.location.fileId()) {
                        String path;
                        for (uint32_t fileId : paths) {
                            if (!path.isEmpty())
                                path += " -> ";
                            Path p = Location::path(fileId);
                            if (!absolute && p.startsWith(projectPath)) {
                                path += p.mid(projectPath.size());
                            } else {
                                path += p;
                            }
                        }
                        if (!write(path))
                            done = true;
                    } else if (paths.size() < static_cast<size_t>(maxDepth)) {
                        for (const auto &includeNode : n->includes) {
                            process(includeNode.second);
                        }
                    }
                    paths.removeLast();
                }
            };
            process(depNode);
        }
    }

    return 0;
}
