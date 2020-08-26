/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef CompletionThread_h
#define CompletionThread_h

#include <clang-c/Index.h>
#include <stddef.h>
#include <stdint.h>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <utility>

#include "Location.h"
#include "rct/Connection.h"
#include "rct/EmbeddedLinkedList.h"
#include "rct/Flags.h"
#include "rct/LinkedList.h"
#include "rct/Map.h"
#include "rct/Thread.h"
#include "Source.h"
#include "RTags.h"
#include "rct/Hash.h"
#include "rct/List.h"
#include "rct/Set.h"
#include "rct/String.h"
#include "rct/Value.h"

struct MatchResult;
class Project;

class CompletionThread : public Thread
{
public:
    CompletionThread(int cacheSize);
    ~CompletionThread();

    virtual void run() override;
    enum Flag {
        None = 0x00,
        Elisp = 0x01,
        XML = 0x02,
        JSON = 0x04,
        IncludeMacros = 0x08,
        WarmUp = 0x10,
        NoWait = 0x20,
        Diagnose = 0x40
    };
    bool isCached(const std::shared_ptr<Project> &project, uint32_t fileId) const;
    void completeAt(Source &&source, Location location, Flags<Flag> flags, int max,
                    const UnsavedFiles &unsavedFiles, const String &prefix,
                    const std::shared_ptr<Connection> &conn);
    void prepare(Source &&source, const UnsavedFiles &unsavedFiles);
    Source findSource(const Set<uint32_t> &deps) const;
    void reparse(const std::shared_ptr<Project> &project, uint32_t fileId);
    void stop();
    String dump();
private:
    struct Request;

    void processDiagnostics(const Request *request, CXCodeCompleteResults *results, CXTranslationUnit unit);
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
        String toString() const;
        Source source;
        Location location;
        Flags<Flag> flags;
        int max;
        UnsavedFiles unsavedFiles;
        String prefix;
        std::shared_ptr<Connection> conn;
    };
    LinkedList<Request*> mPending;
    struct Dump {
        bool done;
        std::mutex mutex;
        std::condition_variable cond;
        String string;
    } *mDump;

    struct Completions {
        Completions(Location loc) : location(loc), next(nullptr), prev(nullptr) {}
        struct Candidate {
            String completion, signature, annotation, parent, briefComment;
            int priority = 0;
#ifdef RTAGS_COMPLETION_TOKENS_ENABLED
            int distance = 0;
#endif
            CXCursorKind cursorKind;
            struct Chunk {
                Chunk()
                    : kind(CXCompletionChunk_Optional)
                {}
                Chunk(String &&t, CXCompletionChunkKind k)
                    : text(std::forward<String>(t)), kind(k)
                {}
                String text;
                CXCompletionChunkKind kind;
            };
            List<Chunk> chunks;

            enum Flag {
                Flag_None = 0x0,
                Flag_IncludeChunks = 0x1
            };
            Value toValue(unsigned int flags) const;
        };

        List<Candidate> candidates;
        const Location location;
        Flags<Flag> flags;
        Completions *next, *prev;
    };

    void printCompletions(const List<std::unique_ptr<MatchResult> > &results, Request *request);
    static bool compareCompletionCandidates(const Completions::Candidate *l,
                                            const Completions::Candidate *r);

    struct SourceFile {
        SourceFile()
            : lastModified(0), parseTime(0), reparseTime(0), codeCompleteTime(0), completions(0), next(nullptr), prev(nullptr)
        {}
        std::shared_ptr<RTags::TranslationUnit> translationUnit;
        UnsavedFiles unsavedFiles;
        uint64_t lastModified;
        uint64_t parseTime, reparseTime, codeCompleteTime; // ms
        size_t completions;
        Source source;
        SourceFile *next, *prev;
    };

#ifdef RTAGS_COMPLETION_TOKENS_ENABLED
    struct Token
    {
        Token(const char *bytes = 0, int size = 0)
            : data(bytes), length(size)
        {}

        inline bool operator==(const Token &other) const
        {
            return length == other.length && !strncmp(data, other.data, length);
        }
        inline bool operator<(const Token &other) const
        {
            if (!data)
                return !other.data ? 0 : -1;
            if (!other.data)
                return 1;
            const int minLength = std::min(length, other.length);
            int ret = memcmp(data, other.data, minLength);
            if (!ret) {
                if (length < other.length) {
                    ret = -1;
                } else if (other.length < length) {
                    ret = 1;
                }
            }
            return ret;
        }

        const char *data;
        int length;

        static inline Map<Token, int> tokenize(const char *data, int size)
        {
            Map<Token, int> tokens;
            int tokenEnd = -1;
            for (int i=size - 1; i>=0; --i) {
                if (RTags::isSymbol(data[i])) {
                    if (tokenEnd == -1)
                        tokenEnd = i;
                } else if (tokenEnd != -1) {
                    addToken(data, i + 1, tokenEnd - i, tokens);
                    tokenEnd = -1;
                }
            }
            if (tokenEnd != -1)
                addToken(data, 0, tokenEnd + 1, tokens);
            return tokens;
        }
    private:
        static inline void addToken(const char *data, int pos, int len, Map<Token, int> &tokens)
        {
            int &val = tokens[Token(data + pos, len)];
            if (!val)
                val = pos;
        }
    };
#endif

    Hash<uint32_t, SourceFile*> mCacheMap;
    EmbeddedLinkedList<SourceFile*> mCacheList;

    mutable std::mutex mMutex;
    std::condition_variable mCondition;
};

RCT_FLAGS(CompletionThread::Flag);

#endif
