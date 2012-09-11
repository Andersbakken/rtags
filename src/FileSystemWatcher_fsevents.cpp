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

    WatcherEvent(Type t, const List<Path>& p) : Event(t), type(t), paths(p) { }

    Type type;
    List<Path> paths;
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
    List<Path>::const_iterator path = we->paths.begin();
    const List<Path>::const_iterator end = we->paths.end();
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

protected:
    void run();

private:
    static void perform(void* thread);

private:
    Mutex mutex;
    WaitCondition waiter;
    bool started;
    bool stopped;
    WatcherReceiver* receiver;

    CFRunLoopRef loop;
    CFRunLoopSourceRef source;
    FSEventStreamRef fss;
    FSEventStreamEventId since;
    List<Path> paths;
    static void notifyCallback(ConstFSEventStreamRef, void*, size_t, void *,
                               const FSEventStreamEventFlags[],
                               const FSEventStreamEventId[]);
};

WatcherThread::WatcherThread(WatcherReceiver* r)
    : started(false), stopped(false), receiver(r), fss(0)
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
        started = true;
        waiter.wakeOne();
    }
    CFRunLoopRun();
}

void WatcherThread::perform(void* thread)
{
    WatcherThread* watcher = static_cast<WatcherThread*>(thread);
    MutexLocker locker(&watcher->mutex);
    if (watcher->stopped) {
        if (watcher->fss) {
            FSEventStreamStop(watcher->fss);
            FSEventStreamInvalidate(watcher->fss);
        }
        CFRunLoopSourceInvalidate(watcher->source);
        CFRunLoopStop(watcher->loop);
        return;
    }

    if (watcher->fss) {
        // ### might make sense to have multiple streams instead of recreating one for each change
        // ### and then merge them if the stream count reaches a given treshold
        FSEventStreamStop(watcher->fss);
        FSEventStreamInvalidate(watcher->fss);
        if (watcher->paths.empty()) {
            watcher->fss = 0;
            return;
        }
    }

    const int pathSize = watcher->paths.size();
    CFStringRef refs[pathSize];
    for (int i = 0; i < pathSize; ++i)
        refs[i] = CFStringCreateWithCString(kCFAllocatorDefault,
                                            watcher->paths[i].nullTerminated(),
                                            kCFStringEncodingUTF8);
    CFArrayRef list = CFArrayCreate(kCFAllocatorDefault,
                                    reinterpret_cast<const void**>(refs),
                                    pathSize,
                                    0);

    FSEventStreamContext ctx = { 0, watcher, 0, 0, 0 };
    watcher->fss = FSEventStreamCreate(kCFAllocatorDefault,
                                       notifyCallback,
                                       &ctx,
                                       list,
                                       watcher->since,
                                       .5,
                                       kFSEventStreamCreateFlagWatchRoot
                                       | kFSEventStreamCreateFlagIgnoreSelf
                                       | kFSEventStreamCreateFlagFileEvents);

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
    List<Path> created, removed, modified;
    for(size_t i = 0; i < numEvents; ++i) {
        const FSEventStreamEventFlags flags = eventFlags[i];
        if (flags & kFSEventStreamEventFlagHistoryDone)
            continue;
        if (flags & kFSEventStreamEventFlagItemIsFile) {
            if (flags & kFSEventStreamEventFlagItemCreated) {
                created.push_back(Path(paths[i]));
            } else if (flags & kFSEventStreamEventFlagItemRemoved) {
                removed.push_back(Path(paths[i]));
            } else if (flags & (kFSEventStreamEventFlagItemModified
                                | kFSEventStreamEventFlagItemInodeMetaMod)) {
                modified.push_back(Path(paths[i]));
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
    stopped = true;
    CFRunLoopSourceSignal(source);
    CFRunLoopWakeUp(loop);
}

void WatcherThread::waitForStarted()
{
    MutexLocker locker(&mutex);
    if (started)
        return;
    do {
        waiter.wait(&mutex);
    } while (!started);
}

bool WatcherThread::watch(const Path& path)
{
    MutexLocker locker(&mutex);
    List<Path>::const_iterator it = paths.begin();
    const List<Path>::const_iterator end = paths.end();
    while (it != end) {
        if (*it == path)
            return false;
        ++it;
    }
    paths.push_back(path);
    CFRunLoopSourceSignal(source);
    CFRunLoopWakeUp(loop);
    return true;
}

bool WatcherThread::unwatch(const Path& path)
{
    MutexLocker locker(&mutex);
    List<Path>::iterator it = paths.begin();
    const List<Path>::const_iterator end = paths.end();
    while (it != end) {
        if (*it == path) {
            paths.erase(it);
            CFRunLoopSourceSignal(source);
            CFRunLoopWakeUp(loop);
            return false;
        }
        ++it;
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
