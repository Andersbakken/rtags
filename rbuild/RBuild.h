#ifndef RBUILD_H
#define RBUILD_H

#include "MakefileParser.h"
#include "Path.h"
#include "GccArguments.h"
#include "SystemInformation.h"
#include <QObject>
#include <clang-c/Index.h>
#include <leveldb/write_batch.h>

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
private slots:
    void processFile(const GccArguments& arguments);
    void makefileDone();
    void startParse();
private:
    void save();
    void compileAll();
    void precompileAll();
    void compile(const GccArguments& arguments, bool *usedPch = 0);
    enum WriteDataFlag {
        None = 0x0,
        WriteDependencies = 0x1
    };
    void writeData(leveldb::WriteBatch *batch, uint flags);
private:
    Path mMakefile, mSourceDir;
    MakefileParser mParser;
    SystemInformation mSysInfo;
    RBuildPrivate* mData;
    Path mDBPath;
    CXIndex mIndex;
    QList<GccArguments> mFiles;
};

#endif // RBUILD_H
