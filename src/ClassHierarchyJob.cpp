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

#include "ClassHierarchyJob.h"

#include <functional>

#include "Project.h"
#include "Symbol.h"
#include "rct/List.h"
#include "rct/Set.h"
#include "rct/String.h"

class QueryMessage;

ClassHierarchyJob::ClassHierarchyJob(Location loc,
                                     const std::shared_ptr<QueryMessage> &query,
                                     const std::shared_ptr<Project> &project)
    : QueryJob(query, project), location(loc)
{
}

int ClassHierarchyJob::execute()
{
    Symbol symbol = project()->findSymbol(location);
    if (symbol.isNull())
        return 1;
    if (!symbol.isClass() || !symbol.isDefinition())
        symbol = project()->findTarget(symbol.location);
    if (!symbol.isClass())
        return 1;

    typedef std::function<Set<Symbol>(const Symbol &)> FindFunc;
    std::function<void (const Symbol &, const char *title, int, FindFunc)> recurse = [&](const Symbol &sym,
                                                                                         const char *title,
                                                                                         int indent,
                                                                                         FindFunc find)
    {
        auto classes = find(sym);
        if (!indent) {
            if (classes.isEmpty()) {
                return;
            }
            write(title);
            indent = 2;
            write<256>("  %s\t%s",
                       sym.symbolName.constData(),
                       sym.location.toString(locationToStringFlags()).constData());
        } else {
            write<256>("%s%s\t%s",
                       String(indent, ' ').constData(),
                       sym.symbolName.constData(),
                       sym.location.toString(locationToStringFlags()).constData());
        }
        for (const Symbol &c : classes) {
            recurse(c, title, indent + 2, find);
        }
    };

    recurse(symbol, "Superclasses:", 0, [this](const Symbol &sym) {
            Set<Symbol> ret;
            for (const String &usr : sym.baseClasses) {
                for (const auto &s : project()->findByUsr(usr, sym.location.fileId(), Project::ArgDependsOn)) {
                    if (s.isDefinition()) {
                        ret.insert(s);
                        break;
                    }
                }
            }
            return ret;
        });
    recurse(symbol, "Subclasses:", 0, [this](const Symbol &sym) { return project()->findSubclasses(sym); });
    return 0;
}
