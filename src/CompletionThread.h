#ifndef CompletionThread_h
#define CompletionThread_h

/* This file is part of RTags.

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include <rct/Thread.h>
#include <memory>
#include <mutex>
#include <condition_variable>
#include "Location.h"
#include "Source.h"
#include <rct/Map.h>
#include <rct/LinkedList.h>
#include <clang-c/Index.h>

class CompletionThread : public Thread
{
public:
    CompletionThread(int cacheSize);

    virtual void run();
    enum Flag {
        None = 0x0,
        Refresh = 0x1,
        Elisp = 0x2
    };
    void completeAt(const Source &source, const Location &location, unsigned int flags, const String &unsaved);
    void stop();
private:
    struct Request;
    void process(const Request *request);
    void printCompletions(const List<std::pair<String, String> > &completions, const Request *request);

    Set<uint32_t> mWatched;
    bool mShutdown;
    const int mCacheSize;
    struct Request {
        Source source;
        Location location;
        unsigned int flags;
        String unsaved;
    };
    LinkedList<Request*> mPending;
    CXIndex mIndex;

    struct Cache {
        CXTranslationUnit translationUnit;
        size_t unsavedHash;
        Source source;
        Map<Location, List<std::pair<String, String> > > completions;
        // should have LRU<Location, List<std::pair<String, String> >
    };
    // this datastructure is only touched from inside the thread so it doesn't
    // need to be protected by mMutex
    Hash<uint32_t, Cache*> mCache;
    // LinkedList<Cache*> mCacheLRU;

    mutable std::mutex mMutex;
    std::condition_variable mCondition;
};

#endif
