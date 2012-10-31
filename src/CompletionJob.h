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
    CompletionJob(const QueryMessage &query, const shared_ptr<Project> &project);
    void init(CXIndex index, CXTranslationUnit unit, const Path &path, int line, int column, const ByteArray &unsaved);

    virtual void execute();
private:
    CXIndex mIndex;
    CXTranslationUnit mUnit;
    Path mPath;
    int mLine, mColumn;
    ByteArray mUnsaved;
};

#endif
