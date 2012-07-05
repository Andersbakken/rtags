#ifndef FileSystemWatcher_h
#define FileSystemWatcher_h

#include <Path.h>
#include <Map.h>
#include <Mutex.h>
#include <signalslot.h>

class FileSystemWatcher
{
public:
    FileSystemWatcher();
    ~FileSystemWatcher();

    bool watch(const Path &path);
    bool unwatch(const Path &path);
    signalslot::Signal1<const Path&> &removed() { return mRemoved; }
    signalslot::Signal1<const Path&> &modified() { return mModified; }
private:
    Mutex mMutex;
    static void notifyCallback(int, unsigned int, void *user) { reinterpret_cast<FileSystemWatcher*>(user)->notifyReadyRead(); }
    void notifyReadyRead();
    int mFd;
    Map<Path, int> mWatchedByPath;
    Map<int, Path> mWatchedById;
    signalslot::Signal1<const Path&> mRemoved, mModified;

#if !defined(OS_Linux) && !defined(OS_FreeBSD) && !defined(OS_Darwin)
#error "FileSystemWatcher not implemented on this platform"
#endif
};
#endif
