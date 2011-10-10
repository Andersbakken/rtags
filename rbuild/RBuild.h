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
#include "ClangArgs.h"

struct Node;
struct MMapData;
class RBuild : public QObject
{
    Q_OBJECT;
public:
    RBuild(int threadPoolCount, const QList<Path> &stdIncludePaths, QObject* parent = 0);
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
    void parseFile(const Path &path, const ClangArgs &args);
    void load(const Path &path, const GccArguments &args);
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
    int mPreprocessing, mParsing;
    QSet<QByteArray> mStdIncludePaths;

    struct Pch {
        QList<Path> allHeaders, postHeaders, sources;
        ClangArgs clangArgs;
    };
    QList<Pch> mPCHFiles;
};

#endif
