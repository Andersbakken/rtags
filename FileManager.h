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

class ParseThread;
class FileManager : public QThread
{
    Q_OBJECT;
public:
    FileManager(ParseThread *parseThread);
    ~FileManager();
    void addMakefile(const Path &makefile);
    GccArguments arguments(const Path &path, bool *ok = 0) const;
    void getInfo(const Path &path, GccArguments *args,
                 QSet<Path> *dependents, QSet<Path> *dependsOn) const;
    QByteArray dependencyMap() const;
    bool addDependencies(const Path &source, const QSet<Path> &headers);
signals:
    void done();
protected:
    bool event(QEvent *event);
private slots:
    void onMakeFinished(int statusCode);
    void onMakeOutput();
    void onMakeError(QProcess::ProcessError error);
private:
    struct MakefileData {
        Path path, directory;
        QByteArray buffer;
        QSet<Path> seen;
        Path workingDirectory;
    };
    QHash<QProcess *, MakefileData> mMakefiles;

    mutable QMutex mFilesMutex;
    struct FileData {
        GccArguments arguments;
        QSet<Path> dependents;
        QSet<Path> dependsOn;
        // If this is Foo.cpp, dependsOn contains Foo.h,
        // If this is Foo.h, dependents contains Foo.cpp
    };
    friend QDataStream &operator<<(QDataStream &ds, const FileData &fd);
    friend QDataStream &operator>>(QDataStream &ds, FileData &fd);

    QHash<Path, FileData> mFiles;
    ParseThread *mParseThread;
};

#endif
