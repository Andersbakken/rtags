#ifndef RBUILD_H
#define RBUILD_H

#include "MakefileParser.h"
#include "Path.h"
#include "GccArguments.h"
#include "SystemInformation.h"
#include <QObject>

class CollectData;

class RBuild : public QObject
{
    Q_OBJECT
public:
    RBuild(QObject *parent = 0);
    ~RBuild();

    void setDBPath(const Path &path);
    void buildDB(const Path& makefile);
    void updateDB();
private slots:
    void makefileDone();
    void makefileFileReady(const MakefileItem& file);
    void startParse();

private:
    void compile(const GccArguments& arguments);
    void writeData(const QByteArray& filename);

private:
    Path mMakefile;
    MakefileParser mParser;
    SystemInformation mSysInfo;
    CollectData* mData;
    Path mDBFile;
};

#endif // RBUILD_H
