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

#include "FindFileJob.h"

#include <assert.h>
#include <limits.h>
#include <map>
#include <set>
#include <sstream>
#include <utility>
#include <vector>

#include "FileManager.h"
#include "Project.h"
#include "RTags.h"
#include "QueryMessage.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Path.h"
#include "rct/Rct.h"
#include "rct/Set.h"

static Flags<QueryJob::JobFlag> flags(Flags<QueryMessage::Flag> queryFlags)
{
    Flags<QueryJob::JobFlag> flags = QueryJob::QuietJob;
    if (queryFlags & QueryMessage::Elisp)
        flags |= QueryJob::QuoteOutput;
    return flags;
}

FindFileJob::FindFileJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &project)
    : QueryJob(query, project, ::flags(query->flags()))
{
    const String q = query->query();
    if (!q.isEmpty()) {
        if (query->flags() & QueryMessage::MatchRegex) {
            if (query->flags() & QueryMessage::MatchCaseInsensitive) {
                mRegex.assign(q.ref(), std::regex::icase);
            } else {
                mRegex.assign(q.ref());
            }
        } else {
            mPattern = q;
        }
    }
}

int FindFileJob::execute()
{
    std::shared_ptr<Project> proj = project();
    if (!proj) {
        return 1;
    }
    if (!proj->fileManager())
        return 1;
    const Path srcRoot = proj->path();
    assert(srcRoot.endsWith('/'));

    enum Mode {
        All,
        FilePath,
        Regex,
        Pattern,
    } mode = All;

    String::CaseSensitivity cs = String::CaseSensitive;
    if (queryFlags() & QueryMessage::MatchRegex) {
        mode = Regex;
    } else if (!mPattern.isEmpty()) {
        mode = mPattern[0] == '/' ? FilePath : Pattern;
    }
    if (queryFlags() & QueryMessage::MatchCaseInsensitive)
        cs = String::CaseInsensitive;

    String out;
    out.reserve(PATH_MAX);
    const bool absolutePath = queryFlags() & QueryMessage::AbsolutePath;
    if (absolutePath)
        out.append(srcRoot);
    const Files& dirs = proj->files();
    assert(proj->fileManager());
    if (dirs.isEmpty())
        proj->fileManager()->load(FileManager::Synchronous);
    Files::const_iterator dirit = dirs.begin();
    bool foundExact = false;
    const int patternSize = mPattern.size();
    List<String> matches;
    const bool preferExact = queryFlags() & QueryMessage::FindFilePreferExact;
    int ret = 1;
    bool firstElisp = queryFlags() & QueryMessage::Elisp;

    auto writeFile = [this, &firstElisp](const Path &path) {
        if (firstElisp) {
            firstElisp = false;
            if (!write("(list", DontQuote))
                return false;
        }
        return write(path);
    };
    while (dirit != dirs.end()) {
        const Path &dir = dirit->first;
        if (dir.size() < srcRoot.size()) {
            continue;
        } else {
            out.append(dir.constData() + srcRoot.size(), dir.size() - srcRoot.size());
        }

        const Set<String> &files = dirit->second;
        for (Set<String>::const_iterator it = files.begin(); it != files.end(); ++it) {
            const String &key = *it;
            out.append(key);
            bool ok = false;
            switch (mode) {
            case All:
                ok = true;
                break;
            case Regex:
                ok = Rct::contains(out, mRegex);
                break;
            case FilePath:
            case Pattern:
                if (!preferExact) {
                    ok = out.contains(mPattern, cs);
                } else {
                    const int outSize = out.size();
                    const bool exact = (outSize > patternSize && out.endsWith(mPattern) && out.at(outSize - (patternSize + 1)) == '/');
                    if (exact) {
                        ok = true;
                        if (!foundExact) {
                            matches.clear();
                            foundExact = true;
                        }
                    } else {
                        ok = !foundExact && out.contains(mPattern, cs);
                    }
                }
                if (!ok && mode == FilePath) {
                    Path p(out);
                    if (!absolutePath)
                        p.prepend(srcRoot);
                    p.resolve();
                    if (p == mPattern)
                        ok = true;
                }
                break;
            }
            if (ok) {
                ret = 0;

                Path matched = out;
                if (absolutePath)
                    matched.resolve();
                if (preferExact && !foundExact) {
                    matches.append(matched);
                } else {
                    if (!writeFile(matched))
                        return 1;
                }
            }
            out.chop(key.size());
        }
        out.chop(dir.size() - srcRoot.size());
        ++dirit;
    }
    for (List<String>::const_iterator it = matches.begin(); it != matches.end(); ++it) {
        if (!writeFile(*it)) {
            return 1;
        }
    }
    if (queryFlags() & QueryMessage::Elisp && !firstElisp && !write(")", DontQuote))
        return 1;
    return ret;
}
