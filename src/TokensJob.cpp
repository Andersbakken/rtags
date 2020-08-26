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

#include "TokensJob.h"

#include <functional>

#include "Project.h"
#include "QueryMessage.h"
#include "rct/Log.h"
#include "RTags.h"
#include "FileMap.h"
#include "Symbol.h"
#include "Token.h"
#include "rct/Flags.h"
#include "rct/String.h"

TokensJob::TokensJob(const std::shared_ptr<QueryMessage> &query,
                     uint32_t fileId,
                     uint32_t from,
                     uint32_t to,
                     const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj), mFileId(fileId), mFrom(from), mTo(to)
{
}

int TokensJob::execute()
{
    std::shared_ptr<Project> proj = project();
    if (!proj)
        return 1;
    auto map = proj->openTokens(mFileId);
    if (!map)
        return 2;

    const uint32_t count = map->count();
    uint32_t i = 0;
    if (mFrom != 0) {
        i = map->lowerBound(mFrom);
        if (i > 0 && i < count) {
            const Token val = map->valueAt(i - 1);
            if (val.offset + val.length >= mFrom)
                --i;
        }
    }

    std::function<bool(const Token &)> writeToken;
    if (queryFlags() & QueryMessage::Elisp) {
        const char *elispFormat = "(cons %d (list (cons 'length %d) (cons 'kind \"%s\") (cons 'spelling \"%s\")))";
        write("(list");
        if (queryFlags() & QueryMessage::TokensIncludeSymbols) {
            writeToken = [this, &proj, elispFormat](const Token &token) {
                String out = String::format<1024>(elispFormat,
                                                  token.offset, token.length, RTags::tokenKindSpelling(token.kind),
                                                  RTags::elispEscape(token.spelling).constData());
                const Symbol sym = proj->findSymbol(token.location);
                if (!sym.isNull()) {
                    out.chop(2);
                    out << " (cons 'symbol ";
                }
                if (!write(out))
                    return false;

                if (!sym.isNull())
                    return write(sym) && write(")))");
                return true;
            };

        } else {
            writeToken = [this, elispFormat](const Token &token) {
                return write<1024>(elispFormat,
                                   token.offset, token.length, RTags::tokenKindSpelling(token.kind),
                                   RTags::elispEscape(token.spelling).constData());
            };
        }
    } else {
        writeToken = [this](const Token &token) {
            return write(token.toString());
        };
    }

    while (i < count) {
        const Token token = map->valueAt(i++);
        if (token.offset > mTo)
            break;
        if (!writeToken(token))
            return 4;
    }

    if (queryFlags() & QueryMessage::Elisp) {
        write(")");
    }

    return 0;
}
