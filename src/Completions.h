#ifndef Completions_h
#define Completions_h

#include "EventReceiver.h"
#include <clang-c/Index.h>
#include "Mutex.h"
#include "Map.h"
#include "ReadWriteLock.h"
#include "RTags.h"
#include "Location.h"

class Completions : public EventReceiver
{
public:
    Completions(int max);
    ~Completions();
    virtual void event(const Event *event);

    ByteArray completions(const Location &loc, unsigned queryFlags, const ByteArray &unsaved);

    struct Entry {
        Entry()
            : index(0), unit(0), previous(0), next(0), finished(false)
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
        Entry *previous, *next;
        ByteArray unsaved;
        bool finished;
    };
private:
    static ByteArray completion(const Entry *entry, uint32_t offset);
    const int mMax;
    // Entry *mEntriesByUsage;
    Map<Path, Entry*> mEntries;
};

#endif
