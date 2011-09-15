#ifndef ParseThread_h
#define ParseThread_h

#include <QtCore>
#include <clang-c/Index.h>
#include "Path.h"
#include "GccArguments.h"

class FileManager;
class ParseThread : public QThread
{
    Q_OBJECT
public:
    ParseThread(FileManager *fm);
    ~ParseThread();
    void abort();
    void load(const Path &path, const GccArguments &arguments);
signals:
    void fileParsed(const Path &path, void *translationUnit);
    void parseError(const Path &path);
    void dependenciesAdded(const Path &path);
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
    int mCount;
    CXIndex mIndex;
    FileManager *mFileManager;
};

#endif
