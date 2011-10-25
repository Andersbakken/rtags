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
    void save();
    void makefileFileReady(const MakefileItem& file);
    void startParse();

private:
    void compile(const GccArguments& arguments);
    void writeData(const QByteArray& filename);

private:
    Path mMakefile;
    MakefileParser mParser;
    SystemInformation mSysInfo;
    RBuildPrivate* mData;
    Path mDBPath;
    CXIndex mIndex;
};

#endif // RBUILD_H
