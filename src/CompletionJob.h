#ifndef CompletionJob_h
#define CompletionJob_h

#include "Job.h"
#include "List.h"
#include "ByteArray.h"
#include "Path.h"
#include "AbortInterface.h"
#include "Event.h"
#include <clang-c/Indexer.h>

class CompletionJob : public Job
{
public:
    CompletionJob(CXIndex index, CXTranslationUnit unit, const Path &path, int line, int column, const ByteArray &unsaved = ByteArray());
    virtual void execute();
private:
    CXIndex mIndex;
    CXTranslationUnit mUnit;
    const Path mPath;
    const int mLine;
    const int mColumn;
    const ByteArray mUnsaved;
};

#endif
