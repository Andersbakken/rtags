#ifndef CompletionJob_h
#define CompletionJob_h

#include "Job.h"
#include "List.h"
#include "ByteArray.h"
#include "Path.h"
#include "AbortInterface.h"
#include "Completions.h"

class CompletionJob : public Job
{
public:
    struct Entry;
    CompletionJob(int id, const Path &input, const List<ByteArray> &args, const ByteArray &unsaved);
    CompletionJob(int id, Completions::Entry *entry, const ByteArray &unsaved);
    virtual void run();

    Completions::Entry *result;
    const ByteArray unsaved;
};

#endif
