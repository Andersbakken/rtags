#include "Completions.h"
#include "CompletionJob.h"
#include "MutexLocker.h"
#include "Server.h"

Completions::Completions(int max)
    : mMax(max)
{

}

Completions::~Completions()
{
    for (Map<Path, Entry*>::const_iterator it = mEntries.begin(); it != mEntries.end(); ++it) {
        delete it->second;
    }
}
void Completions::event(const Event *event)
{
    switch (event->type()) {
    case CompletionJobFinishedEvent::Type: {
        const CompletionJobFinishedEvent *ev = static_cast<const CompletionJobFinishedEvent*>(event);
        Entry *e = mEntries.value(ev->job->result->input);
        if (e != ev->job->result) {
            delete e;
            return;
        }
        e->finished = true;
        break; }
    }
}

ByteArray Completions::completions(const Location &loc, unsigned queryFlags, const ByteArray &unsaved)
{
    Entry *entry = 0;
    const Path path = loc.path();
    Entry *&e = mEntries[path];
    if (!e) {
        // ### need to kill one of the existing entries if there's too many
        const List<ByteArray> args = RTags::compileArgs(loc.fileId(), Path());
        // ### need to think of something for headers, find the first dependency maybe
        if (args.isEmpty()) {
            error("We're not indexing this file %s", path.constData());
            mEntries.remove(path);
            return ByteArray();
        }

        CompletionJob *job = new CompletionJob(this, path, args, unsaved);
        e = job->result;
        Server::instance()->startJob(job);
    } else {
        if (e->unsaved != unsaved) {
            if (!e->finished) {
                error("We're already parsing %s with different unsaved contents", path.constData());
                // ### maybe spawn a new job and somehow detect that the old one is not interesting
            } else {
                e->unsaved = unsaved;
                e->finished = false;
                CompletionJob *job = new CompletionJob(this, e);
                Server::instance()->startJob(job);
            }
        } else if (e->finished) {
            entry = e;
        } else {
            error("We're already parsing a lock for %s", path.constData());
        }
    }

    if (!entry) {
        return ByteArray();
    }
    const ByteArray result = completion(entry, loc.offset());
    return result;
}
ByteArray Completions::completion(const Entry *entry, uint32_t offset)
{
    assert(entry->unit);
    return ByteArray();
}
