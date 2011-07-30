#ifndef ClangRunnable_h
#define ClangRunnable_h

#include <QtCore>
#include <clang-c/Index.h>
#include "Path.h"
#include "GccArguments.h"

class ParseThread : public QThread
{
    Q_OBJECT
public:
    ParseThread();
    ~ParseThread();
    void abort();
    void addMakefile(const Path &path, const QRegExp &accept, const QRegExp &reject);
    void addFile(const Path &path, const GccArguments &args);
    void reparse(const Path &path);
signals:
    void invalidated(const Path &path);
    void fileParsed(const Path &path, void *translationUnit);
    void parseError(const Path &path);
public slots:
    void onFileChanged(const QString &path);
protected:
    void run();
private:
    QMutex mMutex;
    QWaitCondition mWaitCondition;
    bool mAborted;
    struct File {
        Path path;
        GccArguments arguments;
        File *next;
    } *mFirst, *mLast;
    QHash<Path, QSet<Path> > mDependencies;
    QHash<Path, GccArguments> mFiles;
    CXIndex mIndex;
};

#endif
