#include "CompletionJob.h"
#include "IndexerJob.h"

CompletionJob::CompletionJob(int id, const Path &input, const List<ByteArray> &args, const ByteArray &unsaved)
    : Job(id, CompletionJobPriority), result(new Completions::Entry)

{
    result->index = 0;
    result->unit = 0;
    result->input = input;
    result->args = args;
    result->unsaved = unsaved;
    IndexerJob::prepareClangArguments(result->args, result->input, result->clangArgs, result->pchName, result->clangLine);
}

CompletionJob::CompletionJob(int id, Completions::Entry *entry)
    : Job(id, CompletionJobPriority), result(entry)
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
    } else {
        int res = clang_reparseTranslationUnit(result->unit,
                                               result->unsaved.isEmpty() ? 0 : 1,
                                               &unsavedFile,
                                               clang_defaultReparseOptions(result->unit));
        (void)res;
    }
}

