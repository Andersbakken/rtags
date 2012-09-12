#ifndef FileSystemWatcher_h
#define FileSystemWatcher_h

#include "config.h"
#include <Path.h>
#include <Map.h>
#include <Mutex.h>
#include <SignalSlot.h>
#ifdef HAVE_FSEVENTS
#include <CoreServices/CoreServices.h>

class WatcherThread;
class WatcherReceiver;
#endif

class FileSystemWatcher
{
public:
    FileSystemWatcher();
    ~FileSystemWatcher();

    bool watch(const Path &path);
    bool unwatch(const Path &path);
    signalslot::Signal1<const Path &> &removed() { return mRemoved; }
    signalslot::Signal1<const Path &> &added() { return mAdded; }
    signalslot::Signal1<const Path &> &modified() { return mModified; }
    void clear();
#ifdef HAVE_FSEVENTS
    Set<Path> watchedPaths() const;
#else
    Set<Path> watchedPaths() const { return mWatchedByPath.keys().toSet(); } // ### slow
#endif
private:
#ifdef HAVE_FSEVENTS
    WatcherThread* mWatcher;
    WatcherReceiver* mReceiver;
    friend class WatcherReceiver;
#else
    Mutex mMutex;
    static void notifyCallback(int, unsigned int, void *user) { reinterpret_cast<FileSystemWatcher*>(user)->notifyReadyRead(); }
    void notifyReadyRead();
    int mFd;
    Map<Path, int> mWatchedByPath;
    Map<int, Path> mWatchedById;
#endif
    signalslot::Signal1<const Path&> mRemoved, mModified, mAdded;
};
#endif
