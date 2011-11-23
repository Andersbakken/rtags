#ifndef RBUILD_H
#define RBUILD_H

#include "GccArguments.h"
#include "MakefileParser.h"
#include "Path.h"
#include <QObject>
#include <QThreadPool>
#include <clang-c/Index.h>
#include <leveldb/db.h>
#include <leveldb/write_batch.h>

struct RBuildPrivate;
class Precompile;
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
    void onCompileFinished();
    void onPrecompileFinished(Precompile *pch);
    void save();
private:
    void compileAll();
    void precompileAll();
    void compile(const QList<QByteArray> &args, const Path &file, Precompile *precompile);
    enum WriteDataFlag {
        ExcludePCH = 0x1,
        LookupReferencesFromDatabase = 0x2
    };
    void writeData(leveldb::DB *db, leveldb::WriteBatch *batch, unsigned flags);
private:
    Path mMakefile, mSourceDir;
    MakefileParser mParser;
    RBuildPrivate* mData;
    Path mDBPath;
    CXIndex mIndex;
    QHash<Precompile*, QList<GccArguments> > mFilesByPrecompile;
    QList<GccArguments> mFiles;
    int mPendingJobs;
    QThreadPool mThreadPool;
    friend class CompileRunnable;
    friend class PrecompileRunnable;
};

#endif // RBUILD_H
