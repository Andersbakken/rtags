#include "FileSystemWatcher.h"
#include "EventLoop.h"
#include "MutexLocker.h"
#include "Log.h"
#include "config.h"
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

FileSystemWatcher::FileSystemWatcher()
{
    mFd = kqueue();
    assert(mFd != -1);
    EventLoop::instance()->addFileDescriptor(mFd, EventLoop::Read, notifyCallback, this);
}

FileSystemWatcher::~FileSystemWatcher()
{
    struct kevent change;
    struct timespec nullts = { 0, 0 };

    EventLoop::instance()->removeFileDescriptor(mFd);
    for (Map<Path, int>::const_iterator it = mWatchedByPath.begin(); it != mWatchedByPath.end(); ++it) {
        EV_SET(&change, it->second, EVFILT_VNODE, EV_DELETE, 0, 0, 0);
        if (::kevent(mFd, &change, 1, 0, 0, &nullts) == -1) {
            // bad stuff
            error("FileSystemWatcher::~FileSystemWatcher() kevent failed for '%s' (%d) %s",
                  it->first.constData(), errno, strerror(errno));
        }
        ::close(it->second);
    }
    close(mFd);
}

struct FSUserData
{
    Set<Path> all, added, modified;
    FileSystemWatcher* watcher;
};

static inline uint64_t timespecToInt(timespec spec)
{
    uint64_t t = spec.tv_sec * 1000;
    t += spec.tv_nsec / 1000000;
    return t;
}

Path::VisitResult FileSystemWatcher::scanFiles(const Path& path, void* userData)
{
    FSUserData* u = static_cast<FSUserData*>(userData);
    if (path.isDir()) {
        if (u->watcher->mWatchedByPath.contains(path))
            u->modified.insert(path);
        return Path::Recurse;
    } else if (path.isFile()) {
        struct stat st;
        if (!::stat(path.nullTerminated(), &st)) {
            u->watcher->mTimes[path] = timespecToInt(st.st_mtimespec);
        }
    }
    return Path::Continue;
}

Path::VisitResult FileSystemWatcher::updateFiles(const Path& path, void* userData)
{
    FSUserData* u = static_cast<FSUserData*>(userData);
    if (path.isDir()) {
        return Path::Recurse;
    } else if (path.isFile()) {
        struct stat st;
        if (!::stat(path.nullTerminated(), &st)) {
            const uint64_t time = timespecToInt(st.st_mtimespec);
            Map<Path, uint64_t>::iterator it = u->watcher->mTimes.find(path);
            if (it != u->watcher->mTimes.end()) {
                // ### might skip a beat if the file is saved very quickly
                if (time > it->second) {
                    // modified
                    u->modified.insert(path);
                }
                u->all.remove(it->first);
                it->second = time;
            } else {
                // added
                u->added.insert(path);
                u->watcher->mTimes[path] = time;
            }
        }
    }
    return Path::Continue;
}

void FileSystemWatcher::clear()
{
    MutexLocker locker(&mMutex);

    List<struct kevent> changes;
    changes.resize(mWatchedById.size());

    int pos = 0;
    Map<int, Path>::const_iterator it = mWatchedById.begin();
    const Map<int, Path>::const_iterator end = mWatchedById.end();
    while (it != end) {
        EV_SET(&changes[pos++], it->first, EVFILT_VNODE, EV_DELETE, 0, 0, 0);
        ::close(it->first);
        ++it;
    }
    assert(pos == mWatchedById.size());

    mWatchedById.clear();
    mWatchedByPath.clear();
    mTimes.clear();

    struct timespec nullts = { 0, 0 };
    ::kevent(mFd, changes.data(), changes.size(), 0, 0, &nullts);
}

bool FileSystemWatcher::isWatching(const Path& p) const
{
    Path path = p, parent;
    for (;;) {
        if (mWatchedByPath.contains(path))
            return true;
        parent = path.parentDir();
        if (parent == path || parent.isEmpty())
            break;
        path = parent;
    }
    return false;
}

bool FileSystemWatcher::watch(const Path &p)
{
    Path path = p;
    assert(!path.isEmpty());
    MutexLocker lock(&mMutex);
    const Path::Type type = path.type();
    uint32_t flags = 0;
    switch (type) {
    case Path::File:
        path = path.parentDir();
        // fall through
    case Path::Directory:
        flags = NOTE_RENAME|NOTE_DELETE|NOTE_EXTEND|NOTE_WRITE|NOTE_ATTRIB|NOTE_REVOKE;
        break;
    default:
        error("FileSystemWatcher::watch() '%s' doesn't not seem to be watchable", path.constData());
        return false;
    }

    if (!path.endsWith('/'))
        path += '/';
    if (isWatching(path)) {
        return false;
    }
    int ret = ::open(path.nullTerminated(), O_RDONLY);
    //static int cnt = 0;
    //printf("wanting to watch [%05d] %s : %d\n", ++cnt, path.nullTerminated(), ret);
    if (ret != -1) {
        struct kevent change;
        struct timespec nullts = { 0, 0 };
        EV_SET(&change, ret, EVFILT_VNODE, EV_ADD|EV_ENABLE|EV_CLEAR, flags, 0, 0);
        if (::kevent(mFd, &change, 1, 0, 0, &nullts) == -1) {
            // bad things have happened
            error("FileSystemWatcher::watch() kevent failed for '%s' (%d) %s",
                  path.constData(), errno, strerror(errno));
            ::close(ret);
            return false;
        }
    }
    if (ret == -1) {
        error("FileSystemWatcher::watch() watch failed for '%s' (%d) %s",
              path.constData(), errno, strerror(errno));
        return false;
    }

    mWatchedByPath[path] = ret;
    mWatchedById[ret] = path;

    FSUserData data;
    data.watcher = this;
    path.visit(scanFiles, &data);

    // did we watch any parent directories of what we've already watched?
    // if so, unwatch the subdirectories
    int wd;
    struct kevent change;
    struct timespec nullts = { 0, 0 };
    for (Set<Path>::const_iterator it = data.modified.begin(); it != data.modified.end(); ++it) {
        if (mWatchedByPath.remove(*it, &wd)) {
            EV_SET(&change, wd, EVFILT_VNODE, EV_DELETE, 0, 0, 0);
            if (::kevent(mFd, &change, 1, 0, 0, &nullts) == -1) {
                // bad stuff
                error("FileSystemWatcher::watch() kevent (for existing path) failed for '%s' (%d) %s",
                      it->constData(), errno, strerror(errno));
            }
            ::close(wd);
            mWatchedById.remove(wd);
        }
    }

    return true;
}

bool FileSystemWatcher::unwatch(const Path &p)
{
    MutexLocker lock(&mMutex);
    Path path = p;
    if (path.isFile())
        path = path.parentDir();
    int wd = -1;
    if (mWatchedByPath.remove(path, &wd)) {
        debug("FileSystemWatcher::unwatch(\"%s\")", path.constData());
        mWatchedById.remove(wd);
        struct kevent change;
        struct timespec nullts = { 0, 0 };
        EV_SET(&change, wd, EVFILT_VNODE, EV_DELETE, 0, 0, 0);
        if (::kevent(mFd, &change, 1, 0, 0, &nullts) == -1) {
            // bad stuff
            error("FileSystemWatcher::unwatch() kevent failed for '%s' (%d) %s",
                  path.constData(), errno, strerror(errno));
        }
        ::close(wd);
        Map<Path, uint64_t>::iterator it = mTimes.lower_bound(path);
        while (it != mTimes.end() && it->first.startsWith(path)) {
            mTimes.erase(it++);
        }
        return true;
    }
    return false;
}

void FileSystemWatcher::notifyReadyRead()
{
    FSUserData data;
    {
        enum { MaxEvents = 5 };
        MutexLocker lock(&mMutex);
        struct kevent events[MaxEvents];
        struct timespec nullts = { 0, 0 };
        int ret;
        for (;;) {
            ret = ::kevent(mFd, 0, 0, events, MaxEvents, &nullts);
            if (ret == 0) {
                break;
            } else if (ret == -1) {
                error("FileSystemWatcher::notifyReadyRead() kevent failed (%d) %s",
                      errno, strerror(errno));
                break;
            }
            assert(ret > 0 && ret <= MaxEvents);
            for (int i = 0; i < ret; ++i) {
                const struct kevent& event = events[i];
                const Path p = mWatchedById.value(event.ident);
                if (event.flags & EV_ERROR) {
                    error("FileSystemWatcher::notifyReadyRead() kevent element failed for '%s' (%ld) %s",
                          p.constData(), event.data, strerror(event.data));
                    continue;
                }
                if (p.isEmpty()) {
                    warning() << "FileSystemWatcher::notifyReadyRead() We don't seem to be watching " << p;
                    continue;
                }
                if (event.fflags & (NOTE_DELETE|NOTE_REVOKE|NOTE_RENAME)) {
                    // our path has been removed
                    const int wd = event.ident;
                    mWatchedById.remove(wd);
                    mWatchedByPath.remove(p);

                    data.all.clear();
                    Map<Path, uint64_t>::iterator it = mTimes.lower_bound(p);
                    while (it != mTimes.end() && it->first.startsWith(p)) {
                        data.all.insert(it->first);
                        mTimes.erase(it++);
                    }

                    struct kevent change;
                    struct timespec nullts = { 0, 0 };
                    EV_SET(&change, wd, EVFILT_VNODE, EV_DELETE, 0, 0, 0);
                    ::kevent(mFd, &change, 1, 0, 0, &nullts);
                    ::close(wd);
                } else {
                    assert(p.exists());
                    // Figure out what has been changed
                    data.watcher = this;
                    data.added.clear();
                    data.modified.clear();
                    data.all.clear();
                    Map<Path, uint64_t>::iterator it = mTimes.lower_bound(p);
                    while (it != mTimes.end() && it->first.startsWith(p)) {
                        data.all.insert(it->first);
                        ++it;
                    }
                    //printf("before updateFiles, path %s, all %d\n", p.nullTerminated(), data.all.size());
                    p.visit(updateFiles, &data);
                    //printf("after updateFiles, added %d, modified %d, removed %d\n",
                    //       data.added.size(), data.modified.size(), data.all.size());

                    struct {
                        signalslot::Signal1<const Path&> &signal;
                        const Set<Path> &paths;
                    } signals[] = {
                        { mModified, data.modified },
                        { mAdded, data.added }
                    };
                    const unsigned count = sizeof(signals) / sizeof(signals[0]);
                    for (unsigned i=0; i<count; ++i) {
                        for (Set<Path>::const_iterator it = signals[i].paths.begin(); it != signals[i].paths.end(); ++it) {
                            signals[i].signal(*it);
                        }
                    }
                }

                for (Set<Path>::const_iterator it = data.all.begin(); it != data.all.end(); ++it) {
                    mRemoved(*it);
                }
            }
        }
    }
}
