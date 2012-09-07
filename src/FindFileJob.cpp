#include "FindFileJob.h"
#include "RTags.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"
#include "GRTags.h"

FindFileJob::FindFileJob(const QueryMessage &query, const shared_ptr<Project> &project)
    : Job(query, 0, project)
{
    const ByteArray q = query.query();
    if (!q.isEmpty()) {
        if (query.flags() & QueryMessage::PathMatchRegExp) {
            mRegExp = q;
        } else {
            mPattern = q;
        }
    }
}

// #warning not done
void FindFileJob::execute()
{
//     shared_ptr<Project> proj = project();
//     if (!proj || !proj->grtags)
//         return;
//     const Path &srcRoot = proj->srcRoot;

//     enum Mode {
//         All,
//         RegExp,
//         Pattern
//     } mode = All;
//     if (mRegExp.isValid()) {
//         mode = RegExp;
//     } else if (!mPattern.isEmpty()) {
//         mode = Pattern;
//     }
//     ByteArray out;
//     out.reserve(PATH_MAX);
//     if (queryFlags() & QueryMessage::AbsolutePath) {
//         out.append(srcRoot);
//         assert(srcRoot.endsWith('/'));
//     }

//     ScopedDB db = proj->db(Project::GRFiles, ReadWriteLock::Read); // we're using this as a read write lock for GRTags::mFiles
//     const Map<Path, Map<ByteArray, time_t> > &dirs = proj->grtags->mFiles;
//     Map<Path, Map<ByteArray, time_t> >::const_iterator dirit = dirs.begin();
//     while (dirit != dirs.end()) {
//         const Path &dir = dirit->first;
//         out.append(dir.constData() + srcRoot.size(), dir.size() - srcRoot.size());

//         const Map<ByteArray, time_t> &files = dirit->second;
//         for (Map<ByteArray, time_t>::const_iterator it = files.begin(); it != files.end(); ++it) {
//             const ByteArray &key = it->first;
//             out.append(key);
//             bool ok;
//             switch (mode) {
//             case All:
//                 ok = true;
//                 break;
//             case RegExp:
//                 ok = mRegExp.indexIn(out) != -1;
//                 break;
//             case Pattern:
//                 ok = out.contains(mPattern);
//                 break;
//             }
//             if (ok) {
//                 write(out);
//             }
//             out.chop(key.size());
//         }
//         out.chop(dir.size() - srcRoot.size());
//         ++dirit;
//     }
}
