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
    ~RBuild();
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
    bool isFinished() const;
    void preprocess(const Path &sourceFile, const GccArguments &args);
    void parseFile(const Path &path, const GccArguments &args, const char *pchFile);
    void maybePCH();
private slots:
    void maybeDone();
    void onPreprocessorError(const Path &sourceFile, const GccArguments &args, const QByteArray &error);
    void onPreprocessorHeadersFound(const Path &sourceFile, const GccArguments &args, const QList<Path> &headers);
    void onMakeFinished(int statusCode);
    void onMakeOutput();
    void onMakeError(QProcess::ProcessError error);
    void onClangRunnableFinished();
private:
    struct MakefileData {
        Path path, directory;
        QByteArray buffer;
        QStack<Path> dirStack;
        QSet<Path> seen;
    };

    QHash<QProcess *, MakefileData> mMakefiles;
    QThreadPool mThreadPool;
    Path mDatabaseFile;
    DatabaseMode mDatabaseMode;
    int mPreprocessing, mParsing, mFileCount;
    QHash<Path, GccArguments> mParsePending;
    QList<Path> mAllHeaders;
    QSet<QByteArray> mPCHCompilerSwitches;
    QByteArray mPCHFile;
};

#endif
