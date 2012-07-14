#include "CompletionJob.h"
#include "IndexerJob.h"
#include "EventLoop.h"

CompletionJob::CompletionJob(Completions *c, const Path &input, const List<ByteArray> &args, const ByteArray &unsaved)
    : Job(-1, 0, 0), result(new Completions::Entry), completions(c)

{
    result->finished = false;
    result->index = 0;
    result->unit = 0;
    result->input = input;
    result->args = args;
    result->unsaved = unsaved;
    // IndexerJob::prepareClangArguments(result->args, result->input, result->clangArgs, result->pchName, result->clangLine);
}

CompletionJob::CompletionJob(Completions *c, Completions::Entry *entry)
    : Job(-1, 0, 0), result(entry), completions(c)
{
}

void CompletionJob::run()
{
    CXUnsavedFile unsavedFile = { result->unsaved.isEmpty() ? 0 : result->input.constData(),
                                  result->unsaved.isEmpty() ? 0 : result->unsaved.constData(),
                                  result->unsaved.isEmpty() ? 0 : result->unsaved.size() };

    if (!result->index) {
        result->index = clang_createIndex(1, 0);
        result->unit = clang_parseTranslationUnit(result->index, result->input.constData(),
                                                  result->clangArgs.data(), result->clangArgs.size(),
                                                  &unsavedFile, result->unsaved.isEmpty() ? 0 : 1,
                                                  CXTranslationUnit_Incomplete
                                                  |CXTranslationUnit_PrecompiledPreamble
                                                  |CXTranslationUnit_CacheCompletionResults
                                                  |CXTranslationUnit_CXXPrecompiledPreamble);
        error("parsed %d", result->unit != 0);
    } else {
        int res = clang_reparseTranslationUnit(result->unit,
                                               result->unsaved.isEmpty() ? 0 : 1,
                                               &unsavedFile,
                                               clang_defaultReparseOptions(result->unit));
        error("got reparsed %d", res);
        (void)res;
    }
    completions->postEvent(new CompletionJobFinishedEvent(this));
}

