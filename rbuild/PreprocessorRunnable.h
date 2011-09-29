#ifndef PreprocessorRunnable_h
#define PreprocessorRunnable_h

#include <QtCore>
#include "Path.h"
#include "GccArguments.h"

class PreprocessorRunnable : public QObject, public QRunnable
{
    Q_OBJECT
public:
    static void init(const QList<Path> &stdIncludePaths);
    PreprocessorRunnable(const Path &sourceFile, const GccArguments &args);
    virtual void run();
signals:
    void error(const Path &sourceFile, const GccArguments &args, const QByteArray &error);
    void headersFound(const Path &sourceFile, const GccArguments &args, const QList<Path> &headers);
private:
    const Path mSourceFile;
    const GccArguments mArgs;
    static QList<Path> sStdIncludePaths;
};

#endif
