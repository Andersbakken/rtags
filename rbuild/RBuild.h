#ifndef DAEMON_H
#define DAEMON_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QHash>
#include <QThreadPool>
#include <clang-c/Index.h>
#include "Utils.h"
#include "GccArguments.h"
#include "Path.h"
#include "Location.h"

struct Node;
class RBuild : public QObject
{
    Q_OBJECT;
public:
    RBuild(QObject* parent = 0);
    void addMakefile(Path makefile);
    void recurseDir(const Path &path);
private slots:
    void maybeDone();
    void onMakeFinished(int statusCode);
    void onMakeOutput();
    void onMakeError(QProcess::ProcessError error);
    void onClangRunnableFinished();
private:
    struct MakefileData {
        Path path, directory;
        QByteArray buffer;
        QSet<Path> seen;
        Path workingDirectory;
    };
    QHash<QProcess *, MakefileData> mMakefiles;

    struct FileData {
        GccArguments arguments;
        QSet<Path> dependents, dependsOn;
        // If this is Foo.cpp, dependsOn contains Foo.h,
        // If this is Foo.h, dependents contains Foo.cpp
    };
    QHash<Path, FileData> mFiles;
    QThreadPool mThreadPool;
    int mPendingRunnables;
};

#endif
