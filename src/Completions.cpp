#include "Completions.h"
#include "CompletionJob.h"
#include "MutexLocker.h"

Completions::Completions(int max)
    : mMax(max)
{

}

Completions::~Completions()
{
    MutexLocker lock(&mMutex);
    for (Map<Path, Entry*>::const_iterator it = mEntries.begin(); it != mEntries.end(); ++it) {
        delete it->second;
    }
}
void Completions::event(const Event *event)
{

}

ByteArray Completions::completions(const Path &path, unsigned queryFlags, const Map<Path, ByteArray> &unsavedFiles)
{
    if (!path.isFile()) {
        error("%s is not a file", path.constData());
        return ByteArray();
    }
    Entry *entry = 0;
    MutexLocker lock(&mMutex);
    Entry *&e = mEntries[path];
    if (!e) {

        // if (m
        // e = new Entry
    }

    return ByteArray();
}
