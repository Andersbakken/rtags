#ifndef Completions_h
#define Completions_h

#include "EventReceiver.h"
#include <clang-c/Index.h>
#include "Mutex.h"
#include "Map.h"
#include "ReadWriteLock.h"

class Completions : public EventReceiver
{
public:
    Completions(int max);
    ~Completions();
    virtual void event(const Event *event);

    ByteArray completions(const Path &query, unsigned queryFlags, const Map<Path, ByteArray> &unsavedFiles);

    struct Entry {
        Entry()
            : index(0), unit(0), previous(0), next(0)
        {}

        ~Entry()
        {
            if (unit) {
                assert(index);
                clang_disposeTranslationUnit(unit);
                clang_disposeIndex(index);
            }
        }
        CXIndex index;
        CXTranslationUnit unit;
        Path input;
        ByteArray clangLine, pchName;
        List<ByteArray> args;
        List<const char*> clangArgs;
        ReadWriteLock lock;
        Entry *previous, *next;
    };
private:
    const int mMax;
    Mutex mMutex;
    Entry *mEntriesByUsage;
    Map<Path, Entry*> mEntries;
};

#endif
