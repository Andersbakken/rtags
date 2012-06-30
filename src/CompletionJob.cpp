#include "CompletionJob.h"
#include "IndexerJob.h"

CompletionJob::CompletionJob(int id, const Path &input, const List<ByteArray> &args, const ByteArray &u)
    : Job(id, CompletionJobPriority), result(new Entry), unsaved(u)
{
    result->index = 0;
    result->unit = 0;
    result->input = input;
    result->args = args;
    IndexerJob::prepareClangArguments(result->args, result->input, result->clangArgs, result->pchName, result->clangLine);
}

CompletionJob::CompletionJob(int id, Entry *entry, const ByteArray &u)
    : Job(id, CompletionJobPriority), result(entry), unsaved(u)
{

}


void CompletionJob::run()
{
    CXUnsavedFile unsavedFile = { unsaved.isEmpty() ? 0 : result->input.constData(),
                                  unsaved.isEmpty() ? 0 : unsaved.constData(),
                                  unsaved.isEmpty() ? 0 : unsaved.size() };

    if (!result->index) {
        result->index = clang_createIndex(1, 0);
        result->unit = clang_parseTranslationUnit(result->index, result->input.constData(),
                                                  result->clangArgs.data(), result->clangArgs.size(),
                                                  &unsavedFile, unsaved.isEmpty() ? 0 : 1,
                                                  CXTranslationUnit_Incomplete
                                                  |CXTranslationUnit_PrecompiledPreamble
                                                  |CXTranslationUnit_CacheCompletionResults
                                                  |CXTranslationUnit_CXXPrecompiledPreamble);
    } else {
        int res = clang_reparseTranslationUnit(result->unit,
                                               unsaved.isEmpty() ? 0 : 1,
                                               &unsavedFile,
                                               clang_defaultReparseOptions(result->unit));
    }
}

