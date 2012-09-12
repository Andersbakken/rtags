#include "FileSystemWatcher.h"
#include "EventLoop.h"
#include "MutexLocker.h"
#include "Log.h"
#include "config.h"
#include <sys/inotify.h>
#include <sys/ioctl.h>
#include <errno.h>

FileSystemWatcher::FileSystemWatcher()
{
    mFd = inotify_init();
    assert(mFd != -1);
    EventLoop::instance()->addFileDescriptor(mFd, EventLoop::Read, notifyCallback, this);
}

FileSystemWatcher::~FileSystemWatcher()
{
    EventLoop::instance()->removeFileDescriptor(mFd);
    for (Map<Path, int>::const_iterator it = mWatchedByPath.begin(); it != mWatchedByPath.end(); ++it) {
        inotify_rm_watch(mFd, it->second);
    }
    close(mFd);
}

void FileSystemWatcher::clear()
{
    for (Map<Path, int>::const_iterator it = mWatchedByPath.begin(); it != mWatchedByPath.end(); ++it) {
        inotify_rm_watch(mFd, it->second);
    }
    mWatchedByPath.clear();
    mWatchedById.clear();
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
        flags = IN_DELETE_SELF|IN_MOVE_SELF|IN_ATTRIB|IN_DELETE|IN_CLOSE_WRITE;
        break;
    case Path::Directory:
        flags = IN_MOVED_FROM|IN_MOVED_TO|IN_CREATE|IN_DELETE|IN_DELETE_SELF|IN_ATTRIB|IN_CLOSE_WRITE;
        if (!path.endsWith('/'))
            path.append('/');
        break;
    default:
        error("FileSystemWatcher::watch() '%s' doesn't not seem to be watchable", path.constData());
        return false;
    }

    if (mWatchedByPath.contains(path)) {
        return false;
    }
    const int ret = inotify_add_watch(mFd, path.nullTerminated(), flags);
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
        inotify_rm_watch(mFd, wd);
        return true;
    } else {
        return false;
    }
}

static inline void dump(unsigned mask)
{
    if (mask & IN_ACCESS)
        printf("IN_ACCESS ");
    if (mask & IN_MODIFY)
        printf("IN_MODIFY ");
    if (mask & IN_ATTRIB)
        printf("IN_ATTRIB ");
    if (mask & IN_CLOSE_WRITE)
        printf("IN_CLOSE_WRITE ");
    if (mask & IN_CLOSE_NOWRITE)
        printf("IN_CLOSE_NOWRITE ");
    if (mask & IN_CLOSE)
        printf("IN_CLOSE ");
    if (mask & IN_OPEN)
        printf("IN_OPEN ");
    if (mask & IN_MOVED_FROM)
        printf("IN_MOVED_FROM ");
    if (mask & IN_MOVED_TO)
        printf("IN_MOVED_TO ");
    if (mask & IN_CREATE)
        printf("IN_CREATE ");
    if (mask & IN_DELETE)
        printf("IN_DELETE ");
    if (mask & IN_DELETE_SELF)
        printf("IN_DELETE_SELF ");
    if (mask & IN_MOVE_SELF)
        printf("IN_MOVE_SELF ");
}

void FileSystemWatcher::notifyReadyRead()
{
    Set<Path> modified, removed, added;
    {
        MutexLocker lock(&mMutex);
        int s = 0;
        ioctl(mFd, FIONREAD, &s);
        enum { StaticBufSize = 4096 };
        char staticBuf[StaticBufSize];
        char *buf = s > StaticBufSize ? new char[s] : staticBuf;
        const int read = ::read(mFd, buf, s);
        int idx = 0;
        while (idx < read) {
            Path path;
            inotify_event *event = reinterpret_cast<inotify_event*>(buf + idx);
            idx += sizeof(inotify_event) + event->len;
            path = mWatchedById.value(event->wd);
            // printf("%s [%s]", path.constData(), event->name);
            // dump(event->mask);
            // printf("\n");

            const bool isDir = path.isDir();

            if (event->mask & (IN_DELETE_SELF|IN_MOVE_SELF|IN_UNMOUNT)) {
                added.insert(path);
            } else if (event->mask & (IN_CREATE|IN_MOVED_TO)) {
                path.append(event->name);
                added.insert(path);
            } else if (event->mask & (IN_DELETE|IN_MOVED_FROM)) {
                path.append(event->name);
                added.remove(path);
                removed.insert(path);
            } else if (event->mask & (IN_ATTRIB|IN_CLOSE_WRITE)) {
                if (isDir) {
                    path.append(event->name);
                }
                modified.insert(path);
            }
        }
        if (buf != staticBuf)
            delete []buf;
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
