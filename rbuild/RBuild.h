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
struct MMapData;
class RBuild : public QObject
{
    Q_OBJECT;
public:
    RBuild(QObject* parent = 0);
    bool addMakefile(Path makefile);
    void recurseDir(const Path &path);
    enum DatabaseMode {
        Build,
        Update
    };
    bool setDatabaseFile(const Path &path, DatabaseMode mode);
    bool findDatabaseFile(DatabaseMode mode);
    Path databaseFile() const;
    bool initFromDb(const MMapData *data);
    bool addFile(const Path &path, const GccArguments &args);
    bool pendingWork() const;
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
        Path workingDirectory;
    };
    QHash<QProcess *, MakefileData> mMakefiles;
    QHash<Path, QList<GccArguments> > mSeen;
    QThreadPool mThreadPool;
    int mPendingRunnables;
    Path mDatabaseFile;
    DatabaseMode mDatabaseMode;
    bool mPendingWork;
};

#endif
