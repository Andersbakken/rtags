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

#include "IncludePathJob.h"

#include "Project.h"
#include "RTags.h"
#include "Server.h"

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

    if (queryFlags() & QueryMessage::TargetUsrs) {
        const Set<String> usrs = project()->findTargetUsrs(location);
        for (const String &usr : usrs) {
            write(usr);
        }
        return 0;
    }

    auto targets = RTags::sortTargets(project()->findTargets(symbol));

    String allTargetPath;
    for (const auto &target : targets) {
        allTargetPath += project()->dumpIncludePath(location, target) + "\n";
    }

    // debug() << "IncludePathJob::allTargetPath: " << allTargetPath;
    write(allTargetPath);
    return 0;
}
