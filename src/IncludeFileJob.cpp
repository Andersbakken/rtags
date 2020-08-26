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

#include "IncludeFileJob.h"

#include <stdint.h>
#include <string.h>
#include <algorithm>
#include <utility>
#include <vector>

#include "Project.h"
#include "RTags.h"
#include "Location.h"
#include "QueryMessage.h"
#include "Symbol.h"
#include "clang-c/Index.h"
#include "rct/Hash.h"
#include "rct/List.h"
#include "rct/Path.h"
#include "rct/Rct.h"
#include "rct/Set.h"

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

static inline List<Path> headersForSymbol(const std::shared_ptr<Project> &project, Location loc)
{
    List<Path> ret;
    const Path &path = loc.path();
    if (path.isHeader()) {
        ret.append(path);
        if (const DependencyNode *node = project->dependencies().value(loc.fileId())) {
            for (const auto &dependent : node->dependents) {
                const Path p = Location::path(dependent.first);
                if (p.isHeader() && dependent.second->includes.size() == 1) {
                    ret.append(p);
                    // allow headers that only include one header if we don't
                    // find anything for the real header
                }
            }
        }
    }
    return ret;
}

int IncludeFileJob::execute()
{
    if (mSource.isNull()) {
        return 1;
    }
    const Path directory = mSource.sourceFile().parentDir();
    Set<Location> last;
    int matches = 0;
    Set<String> all;
    auto process = [&directory, this, &all](const Set<Location> &locations) {
        for (Location loc : locations) {
            bool first = true;
            for (const Path &path : headersForSymbol(project(), loc)) {
                bool found = false;
                const Symbol sym = project()->findSymbol(loc);
                switch (sym.kind) {
                case CXCursor_ClassDecl:
                case CXCursor_StructDecl:
                case CXCursor_ClassTemplate:
                case CXCursor_TypedefDecl:
                    if (!sym.isDefinition())
                        break;
                    RCT_FALL_THROUGH;
                case CXCursor_FunctionDecl:
                case CXCursor_FunctionTemplate: {
                    List<String> alternatives;
                    if (path.startsWith(directory))
                        alternatives << String::format<256>("#include \"%s\"", path.mid(directory.size()).constData());
                    for (const Source::Include &inc : mSource.includePaths) {
                        const Path p = inc.path.ensureTrailingSlash();
                        if (path.startsWith(p)) {
                            const String str = String::format<256>("#include <%s>", path.mid(p.size()).constData());
                            alternatives << str;
                        }
                    }
                    const int tail = strlen(path.fileName()) + 1;
                    List<String>::const_iterator it = alternatives.begin();
                    while (it != alternatives.end()) {
                        bool drop = false;
                        for (List<String>::const_iterator it2 = it + 1; it2 != alternatives.end(); ++it2) {
                            if (it2->size() < it->size()) {
                                if (!strncmp(it2->constData() + 9, it->constData() + 9, it2->size() - tail - 9)) {
                                    drop = true;
                                    break;
                                }
                            }
                        }
                        if (!drop) {
                            found = true;
                            all.insert(*it);
                        }
                        ++it;
                    }

                    break; }
                default:
                    break;
                }
                if (first) {
                    if (found)
                        break;
                    first = false;
                }
            }
        }
    };
    project()->findSymbols(mSymbol, [&](Project::SymbolMatchType type, const String &symbolName, const Set<Location> &locations) {
            ++matches;
            bool fuzzy = false;
            if (type == Project::StartsWith) {
                fuzzy = true;
                const size_t paren = symbolName.indexOf('(');
                if (paren == mSymbol.size() && !RTags::isFunctionVariable(symbolName))
                    fuzzy = false;
            }

            if (!fuzzy) {
                process(locations);
            } else if (matches == 1) {
                last = locations;
            }
        }, queryFlags());
    if (matches == 1 && !last.isEmpty()) {
        process(last);
    }
    List<String> alternatives = all.toList();
    std::sort(alternatives.begin(), alternatives.end(), [](const String &a, const String &b) {
            return a.size() < b.size();
        });
    for (const auto &a : alternatives) {
        write(a);
    }

    return 0;
}
