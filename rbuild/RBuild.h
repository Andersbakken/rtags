#ifndef RBUILD_H
#define RBUILD_H

#include "GccArguments.h"
#include "MakefileParser.h"
#include "Path.h"
#include <QObject>
#include <QThreadPool>
#include <clang-c/Index.h>

struct RBuildPrivate;
class Precompile;
class RBuild : public QObject
{
    Q_OBJECT
public:
    enum Flag {
        NoFlags = 0x0,
        ClangDisabled = 0x1
    };
    RBuild(unsigned flags, QObject *parent = 0);
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
    void writeData();
    void writeEntities();
    enum Mode {
        Create,
        Update
    };
    bool openDB(Mode mode);
    void closeDB();
private:
    RBuildPrivate* mData;
    friend class CompileRunnable;
    friend class PrecompileRunnable;
};

#endif // RBUILD_H
