#include "FileSystemWatcher.h"
#include "EventLoop.h"
#include "MutexLocker.h"
#include "WaitCondition.h"
#include "Log.h"
#include "Thread.h"
#include "config.h"
#include <errno.h>
#include <CoreFoundation/CoreFoundation.h>

class WatcherThread : public Thread
{
public:
    WatcherThread();

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

    CFRunLoopRef loop;
    CFRunLoopSourceRef source;
    FSEventStreamRef fss;
    FSEventStreamEventId since;
    List<Path> paths;
    static void notifyCallback(ConstFSEventStreamRef, void*, size_t, void *,
                               const FSEventStreamEventFlags[],
                               const FSEventStreamEventId[]);
};

WatcherThread::WatcherThread()
    : started(false), stopped(true), fss(0), since(kFSEventStreamEventIdSinceNow)
{
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

    const void* first = watcher->paths.data();
    CFArrayRef list = CFArrayCreate(kCFAllocatorDefault,
                                    &first,
                                    watcher->paths.size(),
                                    0);

    if (watcher->fss) {
        FSEventStreamStop(watcher->fss);
        FSEventStreamInvalidate(watcher->fss);
    }

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
    for(size_t i = 0; i < numEvents; ++i) {
        printf("path %s modified\n", paths[i]);
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
    mWatcher = new WatcherThread;
    mWatcher->start();
    mWatcher->waitForStarted();
}

FileSystemWatcher::~FileSystemWatcher()
{
    mWatcher->stop();
    mWatcher->join();
    delete mWatcher;
}

bool FileSystemWatcher::watch(const Path &p)
{
    Path path = p;
    const Path::Type type = path.type();
    switch (type) {
    case Path::File:
        break;
    case Path::Directory:
        break;
    default:
        error("FileSystemWatcher::watch() '%s' doesn't not seem to be watchable", path.constData());
        return false;
    }

    return mWatcher->watch(path);
}

bool FileSystemWatcher::unwatch(const Path &path)
{
    if (mWatcher->unwatch(path)) {
        debug("FileSystemWatcher::unwatch(\"%s\")", path.constData());
        return true;
    } else {
        return false;
    }
}

/*
void FileSystemWatcher::notifyReadyRead()
{
    Set<Path> modified, removed, added;

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
*/
