#ifndef RBUILD_H
#define RBUILD_H

#include "GccArguments.h"
#include "MakefileParser.h"
#include "Path.h"
#include <QObject>
#include <QThreadPool>
#include <clang-c/Index.h>

struct RBuildPrivate;
class RBuild : public QObject
{
    Q_OBJECT
public:
    enum Flag {
        NoFlags = 0x0,
        DontIndex = 0x1,
        DontClang = 0x2|DontIndex
    };
    RBuild(unsigned flags, QObject *parent = 0);
    ~RBuild();

    void addIncludePaths(const QList<Path> &path);
    void addDefines(const QList<QByteArray> &define);
    void setDBPath(const Path &path);
    bool buildDB(const Path& makefile, const Path &sourceDir);
    void buildDB(const QList<Path> &sources);
    bool updateDB();
signals:
    void compileFinished();
    void finishedCompiling();
private slots:
    void onMakefileDone();
    void processFile(const GccArguments& arguments);
    void onCompileFinished();
    void save();
private:
    void compile(const QList<QByteArray> &args, const Path &file);
    void writeData();
    void writeEntities();
    enum Mode {
        Create,
        Update
    };
    bool openDB(Mode mode);
    int closeDB();
private:
    RBuildPrivate* mData;
    friend class CompileRunnable;
};

#endif // RBUILD_H
