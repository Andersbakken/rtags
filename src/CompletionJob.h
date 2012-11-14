#ifndef CompletionJob_h
#define CompletionJob_h

#include "Job.h"
#include "List.h"
#include "ByteArray.h"
#include "Path.h"
#include "AbortInterface.h"
#include "Event.h"
#include <clang-c/Index.h>

class CompletionJob : public Job
{
public:
    CompletionJob(const shared_ptr<Project> &project);
    void init(CXIndex index, CXTranslationUnit unit, const Path &path, const List<ByteArray> &args,
              int line, int column, const ByteArray &unsaved);

    virtual void execute();
    signalslot::Signal1<Path> &finished() { return mFinished; }
private:
    CXIndex mIndex;
    CXTranslationUnit mUnit;
    Path mPath;
    List<ByteArray> mArgs;
    int mLine, mColumn;
    ByteArray mUnsaved;
    signalslot::Signal1<Path> mFinished;
};

#endif
