#ifndef FileManager_h
#define FileManager_h

#include <QtCore>
#include "Path.h"
#include "GccArguments.h"

class FileManagerEvent : public QEvent
{
public:
    enum Type {
        WatchPathEvent = User + 1,
        MakefileEvent
    };
    FileManagerEvent(Type type, const Path &path)
        : QEvent(static_cast<QEvent::Type>(type)), mPath(path)
    {
    }
    const Path &path() const { return mPath; }
private:
    const Path mPath;
};


class FileManager : public QThread
{
    Q_OBJECT;
public:
    ~FileManager();
    static FileManager *instance();
    void watchPath(const Path &path);
    void addMakefile(const Path &makefile);
    GccArguments arguments(const Path &path, bool *ok = 0) const;
    void setArguments(const Path &path, const GccArguments &args);
    void clearArguments(const Path &path);
    void addDependency(const Path &path, const Path &dependent);
    void removeDependency(const Path &path, const Path &dependent);
    QSet<Path> dependencies(const Path &path);
    void store();
signals:
    void fileChanged(const Path &path);
protected:
    void run();
    bool event(QEvent *event);
private slots:
    void onFileChanged(const QString &path);
    void onMakeFinished(int statusCode);
    void onMakeOutput();
    void onMakeError(QProcess::ProcessError error);
private:
    FileManager();

    struct MakefileData {
        Path path, directory;
        QByteArray buffer;
        QSet<Path> seen;
        Path workingDirectory;
    };
    QHash<QProcess *, MakefileData> mMakefiles;
    QFileSystemWatcher *mFileSystemWatcher;

    struct FileData {
        FileData(const GccArguments &args = GccArguments()) : lastModified(0) {}
        GccArguments arguments;
        time_t lastModified;
        QSet<Path> dependencies;
        bool watched;
    };

    mutable QReadWriteLock mFilesLock;
    QHash<Path, FileData> mFiles;
};

#endif
