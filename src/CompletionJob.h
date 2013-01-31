#ifndef CompletionJob_h
#define CompletionJob_h

#include "Job.h"
#include <rct/List.h>
#include <rct/String.h>
#include <rct/Path.h>
#include <rct/Event.h>
#include <clang-c/Index.h>

class CompletionJob : public Job
{
public:
    CompletionJob(const shared_ptr<Project> &project);
    void init(CXIndex index, CXTranslationUnit unit, const Path &path, const List<String> &args,
              int line, int column, int pos, const String &unsaved);

    virtual void execute();
    signalslot::Signal1<Path> &finished() { return mFinished; }
private:
    void processDiagnostics(CXCodeCompleteResults* results);

private:
    CXIndex mIndex;
    CXTranslationUnit mUnit;
    Path mPath;
    List<String> mArgs;
    int mLine, mColumn, mPos;
    String mUnsaved;
    signalslot::Signal1<Path> mFinished;
};

#endif
