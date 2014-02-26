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
#include <rct/Connection.h>

class CompletionThread : public Thread
{
public:
    CompletionThread(int cacheSize);
    ~CompletionThread();

    virtual void run();
    enum Flag {
        None = 0x0,
        Refresh = 0x1,
        Elisp = 0x2
    };
    void completeAt(const Source &source, const Location &location, unsigned int flags, const String &unsaved, Connection *conn);
    void stop();
    String dump();
private:
    struct Request;
    void process(Request *request);

    Set<uint32_t> mWatched;
    bool mShutdown;
    const int mCacheSize;
    struct Request {
        ~Request()
        {
            if (conn)
                conn->finish();
        }
        Source source;
        Location location;
        unsigned int flags;
        String unsaved;
        Connection *conn;
    };
    LinkedList<Request*> mPending;
    struct Dump {
        bool done;
        std::mutex mutex;
        std::condition_variable cond;
        String string;
    } *mDump;
    CXIndex mIndex;

    struct Completion {
        Completion(const Location &loc) : location(loc), next(0), prev(0) {}
        struct Node {
            String completion, signature;
            int priority, distance;
            CXCursorKind cursorKind;
        };
        List<Node> completions;
        const Location location;
        Completion *next, *prev;
    };

    void printCompletions(const List<Completion::Node> &completions, Request *request);
    static int compareCompletionNode(const void *left, const void *right);

    struct Cache {
        Cache()
            : translationUnit(0), unsavedHash(0), lastModified(0),
              firstCompletion(0), lastCompletion(0), next(0), prev(0)
        {}
        CXTranslationUnit translationUnit;
        size_t unsavedHash;
        uint64_t lastModified; // ms
        Source source;
        Map<Location, Completion*> completionsMap;
        Completion *firstCompletion, *lastCompletion;
        Cache *next, *prev;
    };
    // this datastructure is only touched from inside the thread so it doesn't
    // need to be protected by mMutex
    Hash<uint32_t, Cache*> mCacheMap;
    Cache *mFirstCache, *mLastCache;

    mutable std::mutex mMutex;
    std::condition_variable mCondition;
};

#endif
