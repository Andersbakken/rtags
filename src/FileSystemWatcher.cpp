#include "FileSystemWatcher.h"
#include "EventLoop.h"
#include "MutexLocker.h"
#ifdef OS_Linux
#include <sys/inotify.h>
#include <errno.h>
#include <sys/ioctl.h>
#endif

FileSystemWatcher::FileSystemWatcher()
{
#ifdef OS_Linux
    mInotifyFd = inotify_init();
    assert(mInotifyFd != -1);
    EventLoop::instance()->addFileDescriptor(mInotifyFd, EventLoop::Read, iNotifyCallback, this);
#endif
}

FileSystemWatcher::~FileSystemWatcher()
{
    EventLoop::instance()->removeFileDescriptor(mInotifyFd);
#ifdef OS_Linux
    for (Map<Path, int>::const_iterator it = mWatchedByPath.begin(); it != mWatchedByPath.end(); ++it) {
        inotify_rm_watch(mInotifyFd, it->second);
    }
    close(mInotifyFd);
#endif
}
bool FileSystemWatcher::watch(const Path &path)
{
#ifdef OS_Linux
    MutexLocker lock(&mMutex);
    const Path::Type type = path.type();
    uint32_t flags = 0;
    switch (type) {
    case Path::File:
        flags = IN_MODIFY|IN_DELETE_SELF|IN_MOVE_SELF|IN_ATTRIB; // ### qt uses IN_MOVE on file which makes no sense to me
        break;
    case Path::Directory:
        flags = IN_MOVE|IN_CREATE|IN_DELETE|IN_DELETE_SELF|IN_ATTRIB;
        break;
    default:
        error("FileSystemWatcher::watch() %s doesn't not seem to be watchable", path.constData());
        return false;
    }

    if (mWatchedByPath.contains(path)) {
        return false;
    }
    errno = 0;
    const int ret = inotify_add_watch(mInotifyFd, path.constData(), flags);
    if (ret == -1) {
        error("FileSystemWatcher::watch() inotify_add_watch failed for %s (%d) %s",
              path.constData(), errno, strerror(errno));
        return false;
    }

    mWatchedByPath[path] = ret;
    mWatchedById[ret] = path;
    return true;
#endif
}
bool FileSystemWatcher::unwatch(const Path &path)
{
#ifdef OS_Linux
    MutexLocker lock(&mMutex);
    int wd = -1;
    if (mWatchedByPath.remove(path, &wd)) {
        debug("FileSystemWatcher::unwatch(\"%s\")", path.constData());
        mWatchedById.remove(wd);
        inotify_rm_watch(mInotifyFd, wd);
        return true;
    } else {
        return false;
    }
#endif
}
#ifdef OS_Linux
void FileSystemWatcher::inotifyReadyRead()
{
    Map<Path, bool> notifications;
    {
        MutexLocker lock(&mMutex);

        // qDebug() << "QInotifyFileSystemWatcherEngine::readFromInotify";

        int s = 0;
        ioctl(mInotifyFd, FIONREAD, &s);
        enum { StaticBufSize = 4096 };
        char staticBuf[StaticBufSize];
        char *buf = s > StaticBufSize ? new char[s] : staticBuf;
        const int read = ::read(mInotifyFd, buf, s);
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
                warning() << "FIONREAD::inotifyReadyRead() We don't seem to be watching " << p;
                continue;
            }
            if (event->mask & (IN_DELETE_SELF|IN_MOVE_SELF|IN_UNMOUNT)) {
                mWatchedById.remove(event->wd);
                mWatchedByPath.remove(p);
                inotify_rm_watch(mInotifyFd, event->wd);
                notifications[p] = true;
            } else {
                notifications[p] = false;
            }
        }
        if (buf != staticBuf)
            delete []buf;
    }
    for (Map<Path, bool>::const_iterator it = notifications.begin(); it != notifications.end(); ++it) {
        if (it->second) {
            emit removed(it->first);
        } else {
            emit modified(it->first);
        }
    }
}
#endif
