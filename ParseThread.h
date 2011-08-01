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
    void addFile(const Path &path, const GccArguments &args, QObject *receiver = 0, const char *member = 0);
    void reparse(const Path &path);
    void loadTranslationUnit(const Path &path, QObject *receiver, const char *member);
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
        QObject *receiver;
        const char *member;
        File *next;
    } *mFirst, *mLast;
    int mCount;
    QHash<Path, QSet<Path> > mDependencies;
    QHash<Path, GccArguments> mFiles;
    CXIndex mIndex;
};

#endif
