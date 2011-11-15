#ifndef RBUILD_H
#define RBUILD_H

#include "MakefileParser.h"
#include "Path.h"
#include "GccArguments.h"
#include "SystemInformation.h"
#include <QObject>
#include <clang-c/Index.h>
#include <leveldb/db.h>
#include <leveldb/write_batch.h>
#ifdef THREADED_COLLECT_SYMBOLS
#include <QThreadPool>
#endif

struct RBuildPrivate;
class RBuild : public QObject
{
    Q_OBJECT
public:
    RBuild(QObject *parent = 0);
    ~RBuild();

    void setDBPath(const Path &path);
    bool buildDB(const Path& makefile, const Path &sourceDir);
    bool updateDB();
signals:
    void compileFinished();
    void finishedCompiling();
private slots:
    void processFile(const GccArguments& arguments);
    void makefileDone();
    void startParse();
    void onCompileFinished();
    void save();
private:
    void compileAll();
    void precompileAll();
    void compile(const GccArguments& arguments, bool *usedPch = 0);
    enum WriteDataFlag {
        ExcludePCH = 0x1,
        LookupReferencesFromDatabase = 0x2
    };
    void writeData(leveldb::DB *db, leveldb::WriteBatch *batch, unsigned flags);
private:
    Path mMakefile, mSourceDir;
    MakefileParser mParser;
    SystemInformation mSysInfo;
    RBuildPrivate* mData;
    Path mDBPath;
    CXIndex mIndex;
    QList<GccArguments> mFiles;
    int mPendingJobs;
#ifdef THREADED_COLLECT_SYMBOLS
    QThreadPool mThreadPool;
    friend class CompileRunnable;
#endif
};

#endif // RBUILD_H
