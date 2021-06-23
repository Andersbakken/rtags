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

#include "FollowLocationJob.h"

#include "Project.h"
#include "RTags.h"
#include "FileMap.h"
#include "QueryMessage.h"
#include "Symbol.h"
#include "clang-c/Index.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Set.h"
#include "rct/String.h"

FollowLocationJob::FollowLocationJob(Location loc,
                                     const std::shared_ptr<QueryMessage> &query,
                                     List<std::shared_ptr<Project>> &&projects)
    : QueryJob(query, std::move(projects)), location(loc)
{
}

int FollowLocationJob::execute()
{
    int idx = 0;
    Symbol symbol;
    for (const auto &project : projects()) {
        symbol = project->findSymbol(location, &idx);
        if (!symbol.isNull()) {
            // if you invoke a destructor explicitly there's a typeref on the class
            // name. This finds the destructor instead.
            if ((symbol.kind == CXCursor_TypeRef || symbol.kind == CXCursor_TemplateRef) && idx > 0) {
                auto symbols = project->openSymbols(location.fileId());
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
            break;
        }
    }
    if (symbol.isNull()) {
        return 1;
    }

    if (queryFlags() & QueryMessage::TargetUsrs) {
        Set<String> usrs;
        for (const auto &project : projects()) {
            usrs += project->findTargetUsrs(location);
        }
        for (const String &usr : usrs) {
            write(usr);
        }
        return 0;
    }

    Set<Symbol> t;
    for (const auto &project : projects()) {
        t += project->findTargets(symbol);
    }
    List<Symbol> targets = RTags::sortTargets(t);


    int rank = -1;
    Set<Location> seen;
    auto writeTarget = [&rank, this, &seen](const Symbol &target) {
        if (seen.insert(target.location)) {
            write(target.location);
            rank = RTags::targetRank(target.kind);
        }
    };
    for (const auto &target : targets) {
        if (target.location.isNull())
            continue;

        if (symbol.usr == target.usr) {
            writeTarget(target);
            if (queryFlags() & QueryMessage::AllTargets)
                continue;
            break;
        }

        if (queryFlags() & QueryMessage::DeclarationOnly ? target.isDefinition() : !target.isDefinition()) {
            Set<Symbol> o;
            for (const auto &project : projects()) {
                o += project->findTargets(target);
            }
            const auto others = RTags::sortTargets(o);
            bool found = false;
            for (auto other : others) {
                if (!other.isNull() && other.usr == target.usr) {
                    found = true;
                    writeTarget(other);
                    if (!(queryFlags() & QueryMessage::AllTargets)) {
                        break;
                    }
                }
            }

            if (found) {
                if (queryFlags() & QueryMessage::AllTargets) {
                    continue;
                } else {
                    break;
                }
            }
        }
        writeTarget(target);
        if (!(queryFlags() & QueryMessage::AllTargets))
            break;
    }
    return 0;
}
