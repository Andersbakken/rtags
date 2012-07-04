#include "FileSystemWatcher.h"
#include "EventLoop.h"
#include "MutexLocker.h"
#include "Log.h"
#if defined(OS_Linux)
#include <sys/inotify.h>
#include <sys/ioctl.h>
#elif defined(OS_FreeBSD)
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <fcntl.h>
#endif
#include <errno.h>

FileSystemWatcher::FileSystemWatcher()
{
#if defined(OS_Linux)
    mFd = inotify_init();
#elif defined(OS_FreeBSD)
    mFd = kqueue();
#endif
    assert(mFd != -1);
    EventLoop::instance()->addFileDescriptor(mFd, EventLoop::Read, notifyCallback, this);
}

FileSystemWatcher::~FileSystemWatcher()
{
    EventLoop::instance()->removeFileDescriptor(mFd);
#if defined(OS_Linux)
    for (Map<Path, int>::const_iterator it = mWatchedByPath.begin(); it != mWatchedByPath.end(); ++it) {
        inotify_rm_watch(mFd, it->second);
    }
    close(mFd);
#endif
}

bool FileSystemWatcher::watch(const Path &path)
{
    MutexLocker lock(&mMutex);
    const Path::Type type = path.type();
    uint32_t flags = 0;
    switch (type) {
#if defined(OS_Linux)
    case Path::File:
        flags = IN_MODIFY|IN_DELETE_SELF|IN_MOVE_SELF|IN_ATTRIB; // ### qt uses IN_MOVE on file which makes no sense to me
        break;
    case Path::Directory:
        flags = IN_MOVE|IN_CREATE|IN_DELETE|IN_DELETE_SELF|IN_ATTRIB;
        break;
#elif defined(OS_FreeBSD)
    case Path::File:
    case Path::Directory:
        flags = NOTE_RENAME|NOTE_DELETE|NOTE_EXTEND|NOTE_WRITE|NOTE_ATTRIB|NOTE_REVOKE;
        break;
#endif
    default:
        error("FileSystemWatcher::watch() %s doesn't not seem to be watchable", path.constData());
        return false;
    }

    if (mWatchedByPath.contains(path)) {
        return false;
    }
#if defined(OS_Linux)
    const int ret = inotify_add_watch(mFd, path.constData(), flags);
#elif defined(OS_FreeBSD)
    int ret = ::open(path.nullTerminated(), O_RDONLY);
    if (ret != -1) {
        struct kevent change;
        struct timespec nullts = { 0, 0 };
        EV_SET(&change, ret, EVFILT_VNODE, EV_ADD|EV_ENABLE|EV_CLEAR, flags, 0, 0);
        ::kevent(mFd, &change, 1, 0, 0, &nullts);
    }
#endif
    if (ret == -1) {
        error("FileSystemWatcher::watch() watch failed for %s (%d) %s",
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
#if defined(OS_Linux)
        inotify_rm_watch(mFd, wd);
#elif defined(OS_FreeBSD)
        struct kevent change;
        struct timespec nullts = { 0, 0 };
        EV_SET(&change, wd, EVFILT_VNODE, EV_DELETE, 0, 0, 0);
        ::kevent(mFd, &change, 1, 0, 0, &nullts);
        ::close(wd);
#endif
        return true;
    } else {
        return false;
    }
}

void FileSystemWatcher::notifyReadyRead()
{
    Map<Path, bool> notifications;
#if defined(OS_Linux)
    {
        MutexLocker lock(&mMutex);

        // qDebug() << "QInotifyFileSystemWatcherEngine::readFromInotify";

        int s = 0;
        ioctl(mFd, FIONREAD, &s);
        enum { StaticBufSize = 4096 };
        char staticBuf[StaticBufSize];
        char *buf = s > StaticBufSize ? new char[s] : staticBuf;
        const int read = ::read(mFd, buf, s);
        int idx = 0;
        Map<int, inotify_event *> events;
        while (idx < read) {
            inotify_event *event = reinterpret_cast<inotify_event*>(buf + idx);
            inotify_event *&ev = events[event->wd];
            if (!ev) {
                ev = event;
            } else {
                ev->mask |= event->mask;
            }
            idx += sizeof(inotify_event) + event->len;
        }
        for (Map<int, inotify_event*>::const_iterator it = events.begin(); it != events.end(); ++it) {
            const inotify_event *event = it->second;
            assert(event->wd == it->first);
            const Path p = mWatchedById.value(event->wd);
            if (p.isEmpty()) {
                warning() << "FileSystemWatcher::notifyReadyRead() We don't seem to be watching " << p;
                continue;
            }
            if (event->mask & (IN_DELETE_SELF|IN_MOVE_SELF|IN_UNMOUNT)) {
                mWatchedById.remove(event->wd);
                mWatchedByPath.remove(p);
                inotify_rm_watch(mFd, event->wd);
                notifications[p] = true;
            } else {
                notifications[p] = false;
            }
        }
        if (buf != staticBuf)
            delete []buf;
    }
#elif defined(OS_FreeBSD)
    {
        enum { MaxEvents = 5 };
        MutexLocker lock(&mMutex);
        struct kevent events[MaxEvents];
        struct timespec nullts = { 0, 0 };
        int ret;
        for (;;) {
            ret = ::kevent(mFd, 0, 0, events, 5, &nullts);
            if (ret == 0) {
                break;
            } else if (ret == -1) {
                error("kevent returned %d, errno %d", ret, errno);
                break;
            }
            assert(ret > 0 && ret <= MaxEvents);
            for (int i = 0; i < ret; ++i) {
                const struct kevent& event = events[i];
                const Path p = mWatchedById.value(event.ident);
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
#endif
    for (Map<Path, bool>::const_iterator it = notifications.begin(); it != notifications.end(); ++it) {
        if (it->second) {
            mRemoved(it->first);
        } else {
            mModified(it->first);
        }
    }
}
