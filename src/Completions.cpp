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
    MutexLocker lock(&mMutex);
    for (Map<uint32_t, Entry*>::const_iterator it = mEntries.begin(); it != mEntries.end(); ++it) {
        delete it->second;
    }
}
void Completions::event(const Event *event)
{

}

ByteArray Completions::completions(const Location &loc, unsigned queryFlags, const Map<Path, ByteArray> &unsavedFiles)
{
    Entry *entry = 0;
    static int id = 0;
    const Path path = loc.path();
    {
        MutexLocker lock(&mMutex);
        Entry *&e = mEntries[loc.fileId()];
        if (!e) {
            // ### need to kill one of the existing entries if there's too many
            const List<ByteArray> args = Rdm::compileArgs(loc.fileId());
            // ### this is not ideal, we hold mMutex longer than we strictly speaking need
            // ### need to think of something for headers, find the first dependency maybe
            if (args.isEmpty()) {
                error("We're not indexing this file %s", path.constData());
                return ByteArray();
            }

            CompletionJob *job = new CompletionJob(++id, path, args, unsavedFiles.value(path));
            e = job->result;
            e->lock.lockForWrite();
            lock.unlock();
            Server::instance()->startJob(job);
        } else if (e->lock.tryLockForRead()) {
            entry = e;
        }
    }

    if (!entry)
        return ByteArray();
    const ByteArray result = completion(entry, loc.offset());
    entry->lock.unlock();
    return result;
}
ByteArray Completions::completion(const Entry *entry, uint32_t offset)
{
    return ByteArray();
}
