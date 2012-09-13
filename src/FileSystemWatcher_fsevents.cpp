#include "FileSystemWatcher.h"
#include "Event.h"
#include "EventReceiver.h"
#include "EventLoop.h"
#include "MutexLocker.h"
#include "WaitCondition.h"
#include "Log.h"
#include "Thread.h"
#include "config.h"
#include <string.h>
#include <errno.h>
#include <CoreFoundation/CoreFoundation.h>

class WatcherEvent : public Event
{
public:
    enum Type { Created = 505, Removed, Modified };

    WatcherEvent(Type t, const Set<Path>& p) : Event(t), type(t), paths(p) { }

    Type type;
    Set<Path> paths;
};

class WatcherReceiver : public EventReceiver
{
public:
    WatcherReceiver(FileSystemWatcher* w);

protected:
    virtual void event(const Event* event);

private:
    FileSystemWatcher* watcher;
};

WatcherReceiver::WatcherReceiver(FileSystemWatcher* w)
    : watcher(w)
{
}

void WatcherReceiver::event(const Event* event)
{
    const WatcherEvent* we = static_cast<const WatcherEvent*>(event);
    Set<Path>::const_iterator path = we->paths.begin();
    const Set<Path>::const_iterator end = we->paths.end();
    while (path != end) {
        switch(we->type) {
        case WatcherEvent::Created:
            watcher->mAdded(*path);
            break;
        case WatcherEvent::Removed:
            watcher->mRemoved(*path);
            break;
        case WatcherEvent::Modified:
            watcher->mModified(*path);
            break;
        }
        ++path;
    }
}

class WatcherThread : public Thread
{
public:
    WatcherThread(WatcherReceiver* r);

    void waitForStarted();
    void stop();

    bool watch(const Path& path);
    bool unwatch(const Path& path);
    void clear();

    Set<Path> watchedPaths() const { MutexLocker locker(&mutex); return paths; }

protected:
    void run();

private:
    static void perform(void* thread);

private:
    mutable Mutex mutex;
    WaitCondition waiter;

    enum Flags {
        Start = 0x1,
        Stop = 0x2,
        Clear = 0x4
    };
    int flags;
    WatcherReceiver* receiver;

    CFRunLoopRef loop;
    CFRunLoopSourceRef source;
    FSEventStreamRef fss;
    FSEventStreamEventId since;
    Set<Path> paths;
    static void notifyCallback(ConstFSEventStreamRef, void*, size_t, void *,
                               const FSEventStreamEventFlags[],
                               const FSEventStreamEventId[]);
};

WatcherThread::WatcherThread(WatcherReceiver* r)
    : flags(0), receiver(r), fss(0)
{
    // ### is this right?
    since = kFSEventStreamEventIdSinceNow;
    CFRunLoopSourceContext ctx;
    memset(&ctx, '\0', sizeof(CFRunLoopSourceContext));
    ctx.info = this;
    ctx.perform = perform;
    source = CFRunLoopSourceCreate(kCFAllocatorDefault, 0, &ctx);
}

void WatcherThread::run()
{
    {
        MutexLocker locker(&mutex);
        loop = CFRunLoopGetCurrent();
        CFRunLoopAddSource(loop, source, kCFRunLoopCommonModes);
        flags |= Start;
        waiter.wakeOne();
    }
    CFRunLoopRun();
}

void WatcherThread::perform(void* thread)
{
    WatcherThread* watcher = static_cast<WatcherThread*>(thread);
    MutexLocker locker(&watcher->mutex);
    if (watcher->flags & Stop) {
        if (watcher->fss) {
            FSEventStreamStop(watcher->fss);
            FSEventStreamInvalidate(watcher->fss);
        }
        CFRunLoopSourceInvalidate(watcher->source);
        CFRunLoopStop(watcher->loop);
        return;
    } else if (watcher->flags & Clear) {
        watcher->flags &= ~Clear;

        if (watcher->fss) {
            FSEventStreamStop(watcher->fss);
            FSEventStreamInvalidate(watcher->fss);
            watcher->fss = 0;
        }

        // We might have paths added since the clear operation was inititated
        if (watcher->paths.empty())
            return;
    }

    // ### might make sense to have multiple streams instead of recreating one for each change
    // ### and then merge them if the stream count reaches a given treshold

    const int pathSize = watcher->paths.size();
    FSEventStreamRef newfss = 0;

    if (pathSize) {
        CFStringRef refs[pathSize];
        int i = 0;
        Set<Path>::const_iterator path = watcher->paths.begin();
        const Set<Path>::const_iterator end = watcher->paths.end();
        while (path != end) {
            // CFStringCreateWithCString copies the string data
            // ### use CFStringCreateWithCStringNoCopy instead?
            refs[i++] = CFStringCreateWithCString(kCFAllocatorDefault,
                                                  path->nullTerminated(),
                                                  kCFStringEncodingUTF8);
            ++path;
        }

        // don't need to hold the mutex beyond this point
        locker.unlock();

        CFArrayRef list = CFArrayCreate(kCFAllocatorDefault,
                                        reinterpret_cast<const void**>(refs),
                                        pathSize,
                                        0);

        FSEventStreamContext ctx = { 0, watcher, 0, 0, 0 };
        newfss = FSEventStreamCreate(kCFAllocatorDefault,
                                     notifyCallback,
                                     &ctx,
                                     list,
                                     watcher->since,
                                     .5,
                                     kFSEventStreamCreateFlagWatchRoot
                                     | kFSEventStreamCreateFlagIgnoreSelf
                                     | kFSEventStreamCreateFlagFileEvents);
    }

    if (!newfss)
        return;

    if (watcher->fss) {
        FSEventStreamStop(watcher->fss);
        FSEventStreamInvalidate(watcher->fss);
    }

    watcher->fss = newfss;

    FSEventStreamScheduleWithRunLoop(watcher->fss, watcher->loop, kCFRunLoopDefaultMode);
    FSEventStreamStart(watcher->fss);
}

void WatcherThread::notifyCallback(ConstFSEventStreamRef streamRef,
                                   void *clientCallBackInfo,
                                   size_t numEvents,
                                   void *eventPaths,
                                   const FSEventStreamEventFlags eventFlags[],
                                   const FSEventStreamEventId eventIds[])
{
    WatcherThread* watcher = static_cast<WatcherThread*>(clientCallBackInfo);
    MutexLocker locker(&watcher->mutex);
    watcher->since = FSEventStreamGetLatestEventId(streamRef);
    char** paths = reinterpret_cast<char**>(eventPaths);
    Set<Path> created, removed, modified;
    for(size_t i = 0; i < numEvents; ++i) {
        const FSEventStreamEventFlags flags = eventFlags[i];
        if (flags & kFSEventStreamEventFlagHistoryDone)
            continue;
        if (flags & kFSEventStreamEventFlagItemIsFile) {
            if (flags & kFSEventStreamEventFlagItemCreated) {
                created.insert(Path(paths[i]));
            } else if (flags & kFSEventStreamEventFlagItemRemoved) {
                removed.insert(Path(paths[i]));
            } else if (flags & (kFSEventStreamEventFlagItemModified
                                | kFSEventStreamEventFlagItemInodeMetaMod)) {
                modified.insert(Path(paths[i]));
            }
        }
    }

    if (!created.empty())
        watcher->receiver->postEvent(new WatcherEvent(WatcherEvent::Created, created));
    if (!removed.empty())
        watcher->receiver->postEvent(new WatcherEvent(WatcherEvent::Removed, removed));
    if (!modified.empty()) {
        watcher->receiver->postEvent(new WatcherEvent(WatcherEvent::Modified, modified));
    }
}

void WatcherThread::stop()
{
    MutexLocker locker(&mutex);
    flags |= Stop;
    CFRunLoopSourceSignal(source);
    CFRunLoopWakeUp(loop);
}

void WatcherThread::waitForStarted()
{
    MutexLocker locker(&mutex);
    if (flags & Start)
        return;
    do {
        waiter.wait(&mutex);
    } while (!(flags & Start));
}

void WatcherThread::clear()
{
    MutexLocker locker(&mutex);
    flags |= Clear;
    paths.clear();
    CFRunLoopSourceSignal(source);
    CFRunLoopWakeUp(loop);
}

bool WatcherThread::watch(const Path& path)
{
    MutexLocker locker(&mutex);
    if (paths.contains(path))
        return false;
    paths.insert(path);
    CFRunLoopSourceSignal(source);
    CFRunLoopWakeUp(loop);
    return true;
}

bool WatcherThread::unwatch(const Path& path)
{
    MutexLocker locker(&mutex);
    if (paths.remove(path)) {
        CFRunLoopSourceSignal(source);
        CFRunLoopWakeUp(loop);
        return true;
    }
    return false;
}

FileSystemWatcher::FileSystemWatcher()
{
    mReceiver = new WatcherReceiver(this);
    mWatcher = new WatcherThread(mReceiver);
    mWatcher->start();
    mWatcher->waitForStarted();
}

FileSystemWatcher::~FileSystemWatcher()
{
    mWatcher->stop();
    mWatcher->join();
    delete mWatcher;
    delete mReceiver;
}

Set<Path> FileSystemWatcher::watchedPaths() const
{
    return mWatcher->watchedPaths();
}

bool FileSystemWatcher::watch(const Path &p)
{
    Path path = p;
    const Path::Type type = path.type();
    switch (type) {
    case Path::File:
        path = path.parentDir();
        break;
    case Path::Directory:
        break;
    default:
        error("FileSystemWatcher::watch() '%s' doesn't not seem to be watchable", path.constData());
        return false;
    }

    return mWatcher->watch(path);
}

bool FileSystemWatcher::unwatch(const Path &p)
{
    Path path = p;
    if (path.isFile())
        path = path.parentDir();
    if (mWatcher->unwatch(path)) {
        debug("FileSystemWatcher::unwatch(\"%s\")", path.constData());
        return true;
    } else {
        return false;
    }
}

void FileSystemWatcher::clear()
{
    mWatcher->clear();
}
