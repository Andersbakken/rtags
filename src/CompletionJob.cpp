#include "CompletionJob.h"
#include "IndexerJob.h"
#include "EventLoop.h"

CompletionJob::CompletionJob(CXIndex index, CXTranslationUnit unit, const Path &path, int line, int column, const ByteArray &unsaved)
    : mIndex(index), mUnit(unit), mPath(path), mLine(line), mColumn(column), mUnsaved(unsaved)
{
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
                                                          clang_defaultCodeCompleteOptions(mUnit));

    clang_disposeCodeCompleteResults(results);
}
