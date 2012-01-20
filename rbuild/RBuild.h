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
        NoFlags = 0x00,
        DontIndex = 0x01,
        DontClang = 0x02|DontIndex,
        DebugAllSymbols = 0x04,
        DisablePCH = 0x08,
        EnableSystemHeaderDependencies = 0x10,
        Verbose = 0x20
    };
    RBuild(unsigned flags, QObject *parent = 0);
    ~RBuild();

    void addIncludePaths(const QList<Path> &path);
    void addDefines(const QList<QByteArray> &define);
    void setDBPath(const Path &path);
    bool buildDB(const QList<Path> &makefiles,
                 const QList<Path> &sourceFiles,
                 const Path &sourceDir);
    bool updateDB();
signals:
    void compileFinished(bool ok);
    void finishedCompiling();
private slots:
    void onMakefileDone();
    void processFile(const GccArguments& arguments);
    void onCompileFinished();
    void save();
private:
    static void indexDeclaration(CXClientData userData, const CXIdxDeclInfo *decl);
    static void indexReference(CXClientData userData, const CXIdxEntityRefInfo *ref);
    static void diagnostic(CXClientData userdata, CXDiagnosticSet set, void *);
    static void getInclusions(CXFile includedFile, CXSourceLocation* inclusionStack,
                              unsigned inclusionStackLen, CXClientData userData);

    bool pch(const GccArguments &pch);
    bool compile(const GccArguments &args, const Path &output = Path());
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
