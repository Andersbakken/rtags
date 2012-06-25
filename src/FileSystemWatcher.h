#ifndef FileSystemWatcher_h
#define FileSystemWatcher_h

#include <QObject>
#include <Path.h>
#include <Map.h>
#include <Mutex.h>

class FileSystemWatcher : public QObject
{
    Q_OBJECT
public:
    FileSystemWatcher();
    ~FileSystemWatcher();

    bool watch(const Path &path);
    bool unwatch(const Path &path);
signals:
    void removed(const Path &path);
    void modified(const Path &path);
private:
#ifdef OS_Linux
    Mutex mMutex;
    int mInotifyFd;
    Map<Path, int> mWatchedByPath;
    Map<int, Path> mWatchedById;
    void inotifyReadyRead();
    static void iNotifyCallback(int, unsigned int, void *user) { reinterpret_cast<FileSystemWatcher*>(user)->inotifyReadyRead(); }
#else
#warning "FileSystemWatcher not implemented on this platform"
#endif
};
#endif
