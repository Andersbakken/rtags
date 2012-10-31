#include "CompletionJob.h"
#include "IndexerJob.h"
#include "EventLoop.h"

CompletionJob::CompletionJob(const QueryMessage &msg, const shared_ptr<Project> &project)
    : Job(msg, 0, project), mIndex(0), mUnit(0), mLine(-1), mColumn(-1)
{}

void CompletionJob::init(CXIndex index, CXTranslationUnit unit, const Path &path, int line, int column, const ByteArray &unsaved)
{
    mIndex = index;
    mUnit = unit;
    mPath = path;
    mLine = line;
    mColumn = column;
    mUnsaved = unsaved;
}

void CompletionJob::execute()
{
    CXUnsavedFile unsavedFile = { mUnsaved.isEmpty() ? 0 : mPath.constData(),
                                  mUnsaved.isEmpty() ? 0 : mUnsaved.constData(),
                                  mUnsaved.size() };

    CXCodeCompleteResults *results = clang_codeCompleteAt(mUnit,
                                                          mPath.constData(),
                                                          mLine, mColumn,
                                                          &unsavedFile,
                                                          mUnsaved.isEmpty() ? 0 : 1,
                                                          clang_defaultCodeCompleteOptions());

    if (results) {
        error() << "Got some results for" << mPath << mLine << mColumn;
        clang_disposeCodeCompleteResults(results);
    }
}
