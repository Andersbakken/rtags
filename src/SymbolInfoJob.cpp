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

#include "SymbolInfoJob.h"

#include <assert.h>
#include <stdint.h>
#include <limits>
#include <utility>

#include "Project.h"
#include "QueryMessage.h"
#include "FileMap.h"
#include "Symbol.h"
#include "rct/Flags.h"
#include "rct/Set.h"
#include "rct/String.h"

SymbolInfoJob::SymbolInfoJob(Location s, Location e,
                             Set<String> &&pieceFilters,
                             const std::shared_ptr<QueryMessage> &query,
                             List<std::shared_ptr<Project>> &&projects)
    : QueryJob(query, std::move(projects)), start(s), end(e)
{
    setPieceFilters(std::move(pieceFilters));
}

int SymbolInfoJob::execute()
{
    if (end.isNull()) {
        Symbol symbol;
        for (const auto &project : projects()) {
            symbol = project->findSymbol(start);
            if (!symbol.isNull())
                break;
        }

        if (!symbol.isNull()) {
            write(symbol);
            return 0;
        }
        return 1;
    }

    assert(start.fileId() == end.fileId());
    int ret = 1;
    for (const auto &project : projects()) {
        auto symbols = project->openSymbols(start.fileId());
        if (symbols && symbols->count()) {
            bool exact = false;
            uint32_t idx = symbols->lowerBound(start, &exact);
            if (exact) {
                if (queryFlags() & QueryMessage::Elisp)
                    write("(list");
                write(symbols->valueAt(idx++));
                ret = 0;
            } else {
                switch (idx) {
                case 0:
                    break;
                case std::numeric_limits<uint32_t>::max():
                    idx = symbols->count() - 1;
                    break;
                default:
                    --idx;
                    break;
                }
            }
            const uint32_t count = symbols->count();
            while (idx < count) {
                const Location loc = symbols->keyAt(idx);
                if (loc > end)
                    break;
                if (loc >= start) {
                    if (ret && queryFlags() & QueryMessage::Elisp)
                        write("(list");
                    write(symbols->valueAt(idx));
                    ret = 0;
                }
                ++idx;
            }
            if (!ret && queryFlags() & QueryMessage::Elisp) {
                write(")");
                break; // ideally this would maybe use a set of symbols to check
                       // if we'd already written it and process all projects
            }
        }
}
    return ret;
}
