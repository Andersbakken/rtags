/* This file is part of RTags (http://rtags.net).

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

#ifndef CompletionThread_h
#define CompletionThread_h

#include <clang-c/Index.h>
#include <condition_variable>
#include <memory>
#include <mutex>

#include "Location.h"
#include "rct/Connection.h"
#include "rct/EmbeddedLinkedList.h"
#include "rct/Flags.h"
#include "rct/LinkedList.h"
#include "rct/Map.h"
#include "rct/Thread.h"
#include "Source.h"

class CompletionThread : public Thread
{
public:
    CompletionThread(int cacheSize);
    ~CompletionThread();

    virtual void run() override;
    enum Flag {
        None = 0x0,
        Refresh = 0x1,
        Elisp = 0x2
    };
    void completeAt(const Source &source, const Location &location, Flags<Flag> flags,
                    const String &unsaved, const std::shared_ptr<Connection> &conn);
    void stop();
    String dump();
private:
    struct Request;
    void process(Request *request);

    Set<uint32_t> mWatched;
    bool mShutdown;
    const size_t mCacheSize;
    struct Request {
        ~Request()
        {
            if (conn)
                conn->finish();
        }
        Source source;
        Location location;
        Flags<Flag> flags;
        String unsaved;
        std::shared_ptr<Connection> conn;
    };
    LinkedList<Request*> mPending;
    struct Dump {
        bool done;
        std::mutex mutex;
        std::condition_variable cond;
        String string;
    } *mDump;
    CXIndex mIndex;

    struct Completions {
        Completions(const Location &loc) : location(loc), next(0), prev(0) {}
        struct Candidate {
            String completion, signature, annotation, parent, briefComment;
            int priority, distance;
            CXCursorKind cursorKind;
        };
        List<Candidate> candidates;
        const Location location;
        Completions *next, *prev;
    };

    void printCompletions(const List<Completions::Candidate> &completions, Request *request);
    static bool compareCompletionCandidates(Completions::Candidate *l,
                                            Completions::Candidate *r);

    struct SourceFile {
        SourceFile()
            : translationUnit(0), unsavedHash(0), lastModified(0),
              parseTime(0), reparseTime(0), codeCompleteTime(0), completions(0), next(0), prev(0)
        {}
        CXTranslationUnit translationUnit;
        size_t unsavedHash;
        uint64_t lastModified, parseTime, reparseTime, codeCompleteTime; // ms
        size_t completions;
        Source source;
        Map<Location, Completions*> completionsMap;
        EmbeddedLinkedList<Completions*> completionsList;
        SourceFile *next, *prev;
    };
    // these datastructures are only touched from inside the thread so it doesn't
    // need to be protected by mMutex
    Hash<uint32_t, SourceFile*> mCacheMap;
    EmbeddedLinkedList<SourceFile*> mCacheList;

    mutable std::mutex mMutex;
    std::condition_variable mCondition;
};

RCT_FLAGS(CompletionThread::Flag);

#endif
