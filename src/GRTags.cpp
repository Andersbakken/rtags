#include "GRTags.h"
#include "GRScanJob.h"
#include "Server.h"
#include "Indexer.h"
#include "GRParseJob.h"
#include <math.h>

GRTags::GRTags()
    : mCount(0), mActive(0), mFlags(0), mFilters(Server::instance()->excludeFilter())
{
    mWatcher.added().connect(this, &GRTags::onFileAdded);
    mWatcher.removed().connect(this, &GRTags::onFileRemoved);
}

void GRTags::init(const shared_ptr<Project> &proj, unsigned flags)
{
    if (flags & Parse)
        mWatcher.modified().connect(this, &GRTags::onFileModified);
    mFlags = flags;
    mProject = proj;
    mSrcRoot = proj->srcRoot;
    assert(mSrcRoot.endsWith('/'));
    recurseDirs();
}

void GRTags::enableParsing()
{
    {
        MutexLocker lock(&mMutex);
        if (mFlags & Parse) {
            return;
        }
        mFlags |= Parse;
    }

    recurseDirs();
}

void GRTags::recurseDirs()
{
    GRScanJob *job = new GRScanJob(mSrcRoot, mProject.lock());
    job->finished().connect(this, &GRTags::onRecurseJobFinished);
    Server::instance()->threadPool()->start(job);
}

void GRTags::onRecurseJobFinished(const Map<Path, bool> &paths)
{
    MutexLocker lock(&mMutex);
    //paths are absolute
    shared_ptr<Project> project = mProject.lock();
    assert(project);
    Scope<GRFilesMap&> scope = project->lockGRFilesForWrite();
    GRFilesMap &map = scope.data();
    mWatcher.clear();
    for (Map<Path, bool>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
        const Path parent = it->first.parentDir();
        Map<ByteArray, time_t> &dir = map[parent];
        if (dir.isEmpty())
            mWatcher.watch(parent);
        dir[it->first.fileName()] = 0;
    }
}

void GRTags::onParseJobFinished(GRParseJob *job, const Map<ByteArray, Map<Location, bool> > &entries)
{
    // #warning not done
//     {
//         MutexLocker lock(&mMutex);
//         --mActive;
//         const int idx = mCount - mActive;
//         if (idx % 50 == 0 || idx == mCount) {
//             error("[%3d%%] Tagged %d/%d",
//                   static_cast<int>(round((static_cast<double>(idx) / static_cast<double>(mCount)) * 100.0)), idx, mCount);
//         }

//         // error("[%3d%%] Tagged %s %d/%d. %d entries.",
//         //       static_cast<int>(round((static_cast<double>(idx) / static_cast<double>(mCount)) * 100.0)),
//         //       job->path().constData(), idx, mCount, entries.size());
//         if (mActive == 0)
//             mCount = 0;
//     }

//     const Path &file = job->path();
//     const time_t parseTime = job->parseTime();
//     if (file.lastModified() > parseTime) { // already outdated
//         parse(file, job->flags());
//         return;
//     }
//     shared_ptr<Project> project = mProject.lock();
//     {
//         ScopedDB database = project->db(Project::GRFiles, ReadWriteLock::Write);
//         addFile(file, parseTime, &database);
//     }
//     ScopedDB database = project->db(Project::GR, ReadWriteLock::Write);
//     if (job->flags() & GRParseJob::Dirty) {
//         dirty(Location::fileId(file), database);
//     }

//     // for the Dirty case we could do it in one pass instead of two
//     Batch batch(database);
//     for (Map<ByteArray, Map<Location, bool> >::const_iterator it = entries.begin(); it != entries.end(); ++it) {
//         const Map<Location, bool> val = it->second;
//         Map<Location, bool> old = database->value<Map<Location, bool> >(it->first);
//         for (Map<Location, bool>::const_iterator i = val.begin(); i != val.end(); ++i) {
//             old[i->first] = i->second;
//         }

//         batch.add(it->first, old);
//     }
}

// void GRTags::removeFile(const Path &file)
// {
//     // const Path dir = file.parentDir();
//     // time_t prev = 0;
//     // shared_ptr<Project> project = mProject.lock();
//     // Scope<GRFilesMap&> scope = project->lockGRFilesForWrite();
//     // GRFilesMap &files = scope.data();
//     // Map<ByteArray, time_t> &map = scope().value()[dir];
//     // map.remove(file.fileName(), &prev);
//     // if (map.isEmpty())
//     //     mFiles.remove(dir);
//     // assert(database.database());
//     // const Slice key(file.constData() + mSrcRoot.size(), file.size() - mSrcRoot.size());
//     // database->remove(key);
//     // if ((mFlags & Parse) && prev) {
//     //     database.reset();
//     //     database = project->db(Project::GR, ReadWriteLock::Write);
//     //     assert(database.database());
//     //     dirty(Location::fileId(file), database);
//     // }
// }

// void GRTags::dirty(uint32_t fileId)
// {
//     // assert(db.database());
//     // Batch batch(db);
//     // RTags::Ptr<Iterator> it(db->createIterator());
//     // it->seekToFirst();
//     // while (it->isValid()) {
//     //     Map<Location, bool> val = it->value<Map<Location, bool> >();
//     //     Map<Location, bool>::iterator i = val.begin();
//     //     bool changed = false;
//     //     while (i != val.end()) {
//     //         if (i->first.fileId() == fileId) {
//     //             val.erase(i++);
//     //             changed = true;
//     //         } else {
//     //             ++i;
//     //         }
//     //     }
//     //     if (changed) {
//     //         if (val.isEmpty()) {
//     //             batch.remove(it->key());
//     //         } else {
//     //             batch.add(it->key(), val);
//     //         }
//     //     }
//     //     it->next();
//     // }
// }
// void GRTags::parse(const Path &path, unsigned flags)
// {
//     {
//         MutexLocker lock(&mMutex);
//         ++mCount;
//         ++mActive;
//     }
//     GRParseJob *job = new GRParseJob(path, flags);
//     job->finished().connect(this, &GRTags::onParseJobFinished);
//     Server::instance()->threadPool()->start(job);
// }

// void GRTags::addFile(const Path &file, time_t time)
// {
//     const Path dir = file.parentDir();
//     const Path fileName = file.fileName();
//     shared_ptr<Project> project = mProject.lock();
//     Scope<GRFilesMap&> scope = project->lockGRFilesForWrite();
//     Map<ByteArray, time_t> &files = scope.data()[dir];
//     files[fileName] = time;
//     if (files.size() == 1) {
//         // printf("Watching %s\n", dir.constData());
//         mWatcher->watch(dir);
//     }
// }

void GRTags::onFileModified(const Path &file)
{
    // printf("%s modified\n", file.constData());
    // assert(mFlags & Parse);
    // {
    //     shared_ptr<Project> project = mProject.lock();
    //     ScopedDB database = project->db(Project::GRFiles, ReadWriteLock::Read);
    //     const Path dir = file.parentDir();
    //     const Path fileName = file.fileName();
    //     const Map<ByteArray, time_t> &files = mFiles[dir];
    //     if (!files.value(fileName))
    //         return;
    // }
    // parse(file, GRParseJob::Dirty);
}

void GRTags::onFileAdded(const Path &path)
{
    const GRScanJob::FilterResult res = GRScanJob::filter(path, mFilters);
    if (res == GRScanJob::Directory) {
        recurseDirs();
        return;
    }

    // printf("%s added\n", path.constData());
    shared_ptr<Project> project = mProject.lock();
    Scope<GRFilesMap&> scope = project->lockGRFilesForWrite();
    GRFilesMap &map = scope.data();
    map[path.parentDir()][path.fileName()] = 0;
    // bool source = false;
    // switch (res) {
    // case GRScanJob::Source:
    //     source = mFlags & Parse;
    //     break;
    // case GRScanJob::File:
    //     break;
    // case GRScanJob::Directory:
    //     // printf("Watching %s\n", path.constData());
    //     recurseDirs();
    //     mWatcher.watch(path);
    //     return;
    // case GRScanJob::Filtered:
    //     return;
    // }

    // if (source) {
    //     parse(path, GRParseJob::None);
    // }
}

void GRTags::onFileRemoved(const Path &path)
{
    shared_ptr<Project> project = mProject.lock();
    Scope<GRFilesMap&> scope = project->lockGRFilesForWrite();
    GRFilesMap &map = scope.data();
    if (map.contains(path)) {
        recurseDirs();
        return;
    }
    const Path parent = path.parentDir();
    Map<ByteArray, time_t> &dir = map[parent];
    if (dir.remove(path.fileName()) && dir.isEmpty()) {
        mWatcher.unwatch(parent);
        map.remove(parent);
    }
}
