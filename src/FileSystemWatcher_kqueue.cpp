#include "FileSystemWatcher.h"
#include "EventLoop.h"
#include "MutexLocker.h"
#include "Log.h"
#include "config.h"
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
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

bool FileSystemWatcher::watch(const Path &p)
{
    Path path = p;
    assert(!path.isEmpty());
    MutexLocker lock(&mMutex);
    const Path::Type type = path.type();
    uint32_t flags = 0;
    switch (type) {
    case Path::File:
    case Path::Directory:
        flags = NOTE_RENAME|NOTE_DELETE|NOTE_EXTEND|NOTE_WRITE|NOTE_ATTRIB|NOTE_REVOKE;
        break;
    default:
        error("FileSystemWatcher::watch() '%s' doesn't not seem to be watchable", path.constData());
        return false;
    }

    if (mWatchedByPath.contains(path)) {
        return false;
    }
    int ret = ::open(path.nullTerminated(), O_RDONLY);
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
    return true;
}

bool FileSystemWatcher::unwatch(const Path &path)
{
    MutexLocker lock(&mMutex);
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
        return true;
    } else {
        return false;
    }
}

void FileSystemWatcher::notifyReadyRead()
{
    Set<Path> modified, removed, added;
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
                    const int wd = event.ident;
                    mWatchedById.remove(wd);
                    mWatchedByPath.remove(p);
                    struct kevent change;
                    struct timespec nullts = { 0, 0 };
                    EV_SET(&change, wd, EVFILT_VNODE, EV_DELETE, 0, 0, 0);
                    ::kevent(mFd, &change, 1, 0, 0, &nullts);
                    ::close(wd);
                    notifications[p] = true;
                } else {
                    notifications[p] = false;
                }
            }
        }
    }

    struct {
        signalslot::Signal1<const Path&> &signal;
        const Set<Path> &paths;
    } signals[] = {
        { mModified, modified },
        { mRemoved, removed },
        { mAdded, added }
    };
    const unsigned count = sizeof(signals) / sizeof(signals[0]);
    for (unsigned i=0; i<count; ++i) {
        for (Set<Path>::const_iterator it = signals[i].paths.begin(); it != signals[i].paths.end(); ++it) {
            signals[i].signal(*it);
        }
    }
    // error() << modified << removed << added;
}
