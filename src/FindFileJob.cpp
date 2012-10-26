#include "FindFileJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"
#include "FileManager.h"

FindFileJob::FindFileJob(const QueryMessage &query, const shared_ptr<Project> &project)
    : Job(query, WriteBuffered, project)
{
    const ByteArray q = query.query();
    if (!q.isEmpty()) {
        if (query.flags() & QueryMessage::MatchRegexp) {
            mRegExp = q;
        } else {
            mPattern = q;
        }
    }
}

void FindFileJob::execute()
{
    shared_ptr<Project> proj = project();
    if (!proj || !proj->fileManager) {
        return;
    }
    const Path srcRoot = proj->srcRoot();

    enum Mode {
        All,
        RegExp,
        Pattern
    } mode = All;
    ByteArray::CaseSensitivity cs = ByteArray::CaseSensitive;
    if (mRegExp.isValid()) {
        mode = RegExp;
    } else if (!mPattern.isEmpty()) {
        mode = Pattern;
    }
    if (queryFlags() & QueryMessage::MatchCaseInsensitive)
        cs = ByteArray::CaseInsensitive;

    ByteArray out;
    out.reserve(PATH_MAX);
    if (queryFlags() & QueryMessage::AbsolutePath) {
        out.append(srcRoot);
        assert(srcRoot.endsWith('/'));
    }
    Scope<const FilesMap&> scope = proj->lockFilesForRead();
    const Map<Path, Set<ByteArray> > &dirs = scope.data();
    Map<Path, Set<ByteArray> >::const_iterator dirit = dirs.begin();
    bool foundExact = false;
    const int patternSize = mPattern.size();
    List<ByteArray> matches;
    const bool preferExact = queryFlags() & QueryMessage::FindFilePreferExact;
    while (dirit != dirs.end()) {
        const Path &dir = dirit->first;
        out.append(dir.constData() + srcRoot.size(), dir.size() - srcRoot.size());

        const Set<ByteArray> &files = dirit->second;
        for (Set<ByteArray>::const_iterator it = files.begin(); it != files.end(); ++it) {
            const ByteArray &key = *it;
            out.append(key);
            bool ok;
            switch (mode) {
            case All:
                ok = true;
                break;
            case RegExp:
                ok = mRegExp.indexIn(out) != -1;
                break;
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
                break;
            }
            if (ok) {
                if (preferExact && !foundExact) {
                    matches.append(out);
                } else {
                    write(out);
                }
            }
            out.chop(key.size());
        }
        out.chop(dir.size() - srcRoot.size());
        ++dirit;
    }
    for (List<ByteArray>::const_iterator it = matches.begin(); it != matches.end(); ++it)
        write(*it);
}
