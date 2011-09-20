#ifndef ParseThread_h
#define ParseThread_h

#include <QtCore>
#include <clang-c/Index.h>
#include "Path.h"
#include "GccArguments.h"

class FileManager;
class VisitThread;
class ParseThread : public QThread
{
    Q_OBJECT
public:
    ParseThread(FileManager *fm, VisitThread *vt);
    ~ParseThread();
    void abort();
    void load(const Path &path);
signals:
    void fileParsed(const Path &path, void *translationUnit);
    void parseError(const Path &path);
    void dependenciesAdded(const QSet<Path> &path);
protected:
    void run();
private:
    QMutex mMutex;
    QWaitCondition mWaitCondition;
    bool mAborted;
    struct File {
        Path path;
        File *next;
    } *mFirst, *mLast;
    QHash<Path, time_t> mFiles;
    int mCount;
    CXIndex mIndex;
    FileManager *mFileManager;
    VisitThread *mVisitThread;
};

#endif
