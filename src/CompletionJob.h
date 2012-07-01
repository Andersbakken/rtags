#ifndef CompletionJob_h
#define CompletionJob_h

#include "Job.h"
#include "List.h"
#include "ByteArray.h"
#include "Path.h"
#include "AbortInterface.h"
#include "Completions.h"
#include "Event.h"

class CompletionJob : public Job
{
public:
    struct Entry;
    CompletionJob(Completions *c, const Path &input, const List<ByteArray> &args, const ByteArray &unsaved);
    CompletionJob(Completions *c, Completions::Entry *entry);
    virtual void run();

    Completions::Entry *result;
    Completions *completions;
};

class CompletionJobFinishedEvent : public Event
{
public:
    enum { Type = 1 };
    CompletionJobFinishedEvent(CompletionJob *j)
        : Event(Type), job(j)
    {}
    CompletionJob *job;
};

#endif
