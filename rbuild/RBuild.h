#ifndef RBUILD_H
#define RBUILD_H

#include "MakefileParser.h"
#include "Path.h"
#include "GccArguments.h"
#include "SystemInformation.h"
#include <QObject>
#include <clang-c/Index.h>

struct RBuildPrivate;
class RBuild : public QObject
{
    Q_OBJECT
public:
    RBuild(QObject *parent = 0);
    ~RBuild();

    void setDBPath(const Path &path);
    void buildDB(const Path& makefile);
    bool updateDB();
private slots:
    void makefileFileReady(const MakefileItem& file);
    void makefileDone();
    void startParse();

private:
    void save();
    void compileAll();
    void precompileAll();
    void processFile(const GccArguments& arguments);
    void compile(const GccArguments& arguments, bool *usedPch = 0);
    void writeData(const QByteArray& filename);

private:
    Path mMakefile;
    MakefileParser mParser;
    SystemInformation mSysInfo;
    RBuildPrivate* mData;
    Path mDBPath;
    CXIndex mIndex;
    QList<GccArguments> mFiles;
};

#endif // RBUILD_H
