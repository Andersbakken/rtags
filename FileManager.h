#ifndef FileManager_h
#define FileManager_h

#include <QtCore>
#include "Path.h"
#include "GccArguments.h"

class FileManagerEvent : public QEvent
{
public:
    enum Type {
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
    void addMakefile(const Path &makefile);
    GccArguments arguments(const Path &path, bool *ok = 0) const;
    void setArguments(const Path &path, const GccArguments &args);
    void clearArguments(const Path &path);
    void store();
protected:
    bool event(QEvent *event);
private slots:
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

    mutable QReadWriteLock mFilesLock;
    QHash<Path, GccArguments> mFiles;
};

#endif
