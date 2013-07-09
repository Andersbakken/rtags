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
    enum Type {
        Stream,
        Sync
    };
    CompletionJob(const shared_ptr<Project> &project, Type type);
    void init(CXIndex index, CXTranslationUnit unit, const Path &path, const List<String> &args,
              int line, int column, int pos, const String &unsaved, int parseCount);

    virtual void execute();
    signalslot::Signal1<Path> &finished() { return mFinished; }
    Type type() const { return mType; }
private:
    void processDiagnostics(CXCodeCompleteResults* results);

private:
    CXIndex mIndex;
    CXTranslationUnit mUnit;
    Path mPath;
    List<String> mArgs;
    int mLine, mColumn, mPos, mParseCount;
    String mUnsaved;
    signalslot::Signal1<Path> mFinished;
    const Type mType;
};

#endif
