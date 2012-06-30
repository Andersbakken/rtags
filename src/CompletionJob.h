#ifndef CompletionJob_h
#define CompletionJob_h

#include "Job.h"
#include "List.h"
#include "ByteArray.h"
#include "Path.h"
#include "AbortInterface.h"
#include <clang-c/Index.h>
#include <memory.h>

class CompletionJob : public Job
{
public:
    struct Entry;
    CompletionJob(int id, const Path &input, const List<ByteArray> &args, const ByteArray &unsaved);
    CompletionJob(int id, Entry *entry, const ByteArray &unsaved);
    virtual void run();

    struct Entry {
        CXIndex index;
        CXTranslationUnit unit;
        Path input;
        ByteArray clangLine, pchName;
        List<ByteArray> args;
        List<const char*> clangArgs;
    } *result;
    const ByteArray unsaved;
};

#endif
