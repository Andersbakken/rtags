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

#include "CompletionThread.h"
#include "RTagsClang.h"
#include "Server.h"
#include "Token.h"

CompletionThread::CompletionThread(int cacheSize)
    : mShutdown(false), mCacheSize(cacheSize), mDump(0), mIndex(0), mFirstCache(0), mLastCache(0)
{
}

CompletionThread::~CompletionThread()
{
    Rct::deleteLinkedListNodes(mFirstCache);
}

void CompletionThread::run()
{
    mIndex = clang_createIndex(0, 0);
    while (true) {
        Request *request = 0;
        Dump *dump = 0;
        {
            std::unique_lock<std::mutex> lock(mMutex);
            while (!mShutdown && mPending.isEmpty() && !mDump) {
                mCondition.wait(lock);
            }
            if (mShutdown) {
                for (auto it = mPending.begin(); it != mPending.end(); ++it) {
                    delete *it;
                }
                mPending.clear();
                if (mDump) {
                    std::unique_lock<std::mutex> lock(mDump->mutex);
                    mDump->done = true;
                    mDump->cond.notify_one();
                    mDump = 0;
                }
                break;
            } else if (mDump) {
                std::swap(dump, mDump);
            } else {
                assert(!mPending.isEmpty());
                request = mPending.takeFirst();
            }
        }
        if (dump) {
            std::unique_lock<std::mutex> lock(dump->mutex);
            Log out(&dump->string);
            for (Cache *cache = mFirstCache; cache; cache = cache->next) {
                out << cache->source << "\nhash:" << cache->unsavedHash
                    << "lastModified:" << cache->lastModified
                    << "translationUnit:" << cache->translationUnit << "\n";
                for (Completion *completion = cache->firstCompletion; completion; completion = completion->next) {
                    out << "    " << completion->location.key() << "\n";
                    for (const auto &c : completion->completions) {
                        out << "        " << c.completion << c.signature << c.priority << c.distance
                            << RTags::eatString(clang_getCursorKindSpelling(c.cursorKind)) << "\n";
                    }
                }
            }
            dump->done = true;
            dump->cond.notify_one();
        } else {
            assert(request);
            process(request);
            delete request;
        }
    }
    clang_disposeIndex(mIndex);
    mIndex = 0;
}

void CompletionThread::completeAt(const Source &source, const Location &location,
                                  unsigned int flags, const String &unsaved, Connection *conn)
{
    Request *request = new Request({ source, location, flags, unsaved, conn});
    std::unique_lock<std::mutex> lock(mMutex);
    auto it = mPending.begin();
    while (it != mPending.end()) {
        if ((*it)->source == source) {
            delete *it;
            it = mPending.erase(it);
        } else {
            ++it;
        }
    }
    mPending.push_front(request);
    mCondition.notify_one();
}

String CompletionThread::dump()
{
    Dump dump = { false };
    {
        std::unique_lock<std::mutex> lock(mMutex);
        if (mDump)
            return String(); // dump in progress

        mDump = &dump;
        mCondition.notify_one();
    }
    std::unique_lock<std::mutex> lock(dump.mutex);
    while (!dump.done) {
        dump.cond.wait(lock);
    }
    return std::move(dump.string);
}

void CompletionThread::stop()
{
    std::unique_lock<std::mutex> lock(mMutex);
    mShutdown = true;
    mCondition.notify_one();
}

static inline bool isPartOfSymbol(char ch)
{
    return isalnum(ch) || ch == '_';
}

int CompletionThread::compareCompletionNode(const void *left, const void *right)
{
    const Completion::Node *l = reinterpret_cast<const Completion::Node*>(left);
    const Completion::Node *r = reinterpret_cast<const Completion::Node*>(right);
    if (l->priority != r->priority)
        return l->priority < r->priority ? -1 : 1;
    if ((l->distance != -1) != (r->distance != -1))
        return l->distance != -1 ? -1 : 1;
    if (l->distance != r->distance)
        return l->distance > r->distance ? -1 : 1;
    return strcmp(l->completion.constData(), r->completion.constData());
}

void CompletionThread::process(Request *request)
{
    StopWatch sw;
    int parseTime = 0;
    int reparseTime = 0;
    int completeTime = 0;
    int processTime = 0;
    Cache *&cache = mCacheMap[request->source.fileId];
    if (cache && cache->source != request->source) {
        delete cache;
        cache = 0;
    }
    if (!cache) {
        cache = new Cache;
        Rct::insertLinkedListNode(cache, mFirstCache, mLastCache, mLastCache);
        while (mCacheMap.size() > mCacheSize) {
            Cache *c = mFirstCache;
            Rct::removeLinkedListNode(c, mFirstCache, mLastCache);
            mCacheMap.remove(c->source.fileId);
            delete c;
        }
    }
    if (cache->translationUnit && cache->source != request->source) {
        clang_disposeTranslationUnit(cache->translationUnit);
        cache->translationUnit = 0;
        cache->source = request->source;
    } else if (!cache->translationUnit) {
        cache->source = request->source;
    }

    const Path sourceFile = request->source.sourceFile();
    CXUnsavedFile unsaved = {
        sourceFile.constData(),
        request->unsaved.constData(),
        static_cast<unsigned long>(request->unsaved.size())
    };

    size_t hash = 0;
    uint64_t lastModified = 0;
    if (request->unsaved.size()) {
        std::hash<String> h;
        hash = h(request->unsaved);
    } else {
        lastModified = sourceFile.lastModifiedMs();
    }

    if (!cache->translationUnit) {
        cache->completionsMap.clear();
        Rct::deleteLinkedListNodes(cache->firstCompletion);
        cache->firstCompletion = cache->lastCompletion = 0;
        sw.restart();
        unsigned int flags = clang_defaultEditingTranslationUnitOptions();
        flags |= CXTranslationUnit_PrecompiledPreamble;
        flags |= CXTranslationUnit_CacheCompletionResults;
        flags |= CXTranslationUnit_SkipFunctionBodies;
        // (CXTranslationUnit_PrecompiledPreamble
        // |CXTranslationUnit_CacheCompletionResults
        // |CXTranslationUnit_SkipFunctionBodies);
        RTags::parseTranslationUnit(sourceFile, request->source.arguments,
                                    List<String>(), // we don't want -fspell-checking and friends
                                    cache->translationUnit, mIndex,
                                    &unsaved, request->unsaved.size() ? 1 : 0, flags);
        parseTime = sw.restart();
        if (cache->translationUnit) {
            RTags::reparseTranslationUnit(cache->translationUnit, &unsaved, request->unsaved.size() ? 1 : 0);
        }
        reparseTime = sw.elapsed();
        if (!cache->translationUnit)
            return;
        cache->unsavedHash = hash;
        cache->lastModified = lastModified;
    } else if (cache->unsavedHash != hash || cache->lastModified != lastModified) {
        cache->completionsMap.clear();
        Rct::deleteLinkedListNodes(cache->firstCompletion);
        cache->firstCompletion = cache->lastCompletion = 0;
        cache->unsavedHash = hash;
        cache->lastModified = lastModified;
    } else if (!(request->flags & Refresh)) {
        const auto it = cache->completionsMap.find(request->location);
        if (it != cache->completionsMap.end()) {
            if (cache->completionsMap.size() > 1) {
                Rct::removeLinkedListNode(it->second, cache->firstCompletion, cache->lastCompletion);
                Rct::insertLinkedListNode(it->second, cache->firstCompletion, cache->lastCompletion, cache->lastCompletion);
            }
            error("Found completions (%d) in cache %s:%d:%d",
                  it->second->completions.size(), sourceFile.constData(),
                  request->location.line(), request->location.column());
            printCompletions(it->second->completions, request);
            return;
        }
    }

    sw.restart();
    CXCodeCompleteResults *results = clang_codeCompleteAt(cache->translationUnit, sourceFile.constData(),
                                                          request->location.line(), request->location.column(),
                                                          &unsaved, unsaved.Length ? 1 : 0,
                                                          CXCodeComplete_IncludeMacros|CXCodeComplete_IncludeCodePatterns);
    completeTime = sw.restart();
    if (results) {
        Completion::Node *nodes = new Completion::Node[results->NumResults];
        int nodeCount = 0;
        Map<Token, int> tokens;
        if (!request->unsaved.isEmpty()) {
            tokens = Token::tokenize(request->unsaved.constData(), request->unsaved.size());
            // for (Map<Token, int>::const_iterator it = tokens.begin(); it != tokens.end(); ++it) {
            //     error() << String(it->first.data, it->first.length) << it->second;
            // }
        }
        for (unsigned i = 0; i < results->NumResults; ++i) {
            const CXCursorKind kind = results->Results[i].CursorKind;
            if (kind == CXCursor_Destructor)
                continue;

            const CXCompletionString &string = results->Results[i].CompletionString;
            const CXAvailabilityKind availabilityKind = clang_getCompletionAvailability(string);
            if (availabilityKind != CXAvailability_Available)
                continue;

            const int priority = clang_getCompletionPriority(string);
            if (priority >= 70)
                continue;

            Completion::Node &node = nodes[nodeCount];
            node.cursorKind = kind;
            node.priority = priority;
            node.signature.reserve(256);
            const int chunkCount = clang_getNumCompletionChunks(string);
            bool ok = true;
            for (int j=0; j<chunkCount; ++j) {
                const CXCompletionChunkKind chunkKind = clang_getCompletionChunkKind(string, j);
                if (chunkKind == CXCompletionChunk_TypedText) {
                    node.completion = RTags::eatString(clang_getCompletionChunkText(string, j));
                    if (node.completion.size() > 8 && node.completion.startsWith("operator") && !isPartOfSymbol(node.completion.at(8))) {
                        ok = false;
                        break;
                    }
                    node.signature.append(node.completion);
                } else {
                    node.signature.append(RTags::eatString(clang_getCompletionChunkText(string, j)));
                    if (chunkKind == CXCompletionChunk_ResultType)
                        node.signature.append(' ');
                }
            }

            if (ok) {
                int ws = node.completion.size() - 1;
                while (ws >= 0 && isspace(node.completion.at(ws)))
                    --ws;
                if (ws >= 0) {
                    node.completion.truncate(ws + 1);
                    node.signature.replace("\n", "");
                    node.distance = tokens.value(Token(node.completion.constData(), node.completion.size()), -1);
                    ++nodeCount;
                    continue;
                }
            }
            node.completion.clear();
            node.signature.clear();
        }
        enum { SendThreshold = 500 };
        if (nodeCount) {
            if (nodeCount <= SendThreshold) {
                qsort(nodes, nodeCount, sizeof(Completion::Node), compareCompletionNode);
                Completion *&c = cache->completionsMap[request->location];
                if (c) {
                    if (cache->completionsMap.size() > 1) {
                        Rct::removeLinkedListNode(c, cache->firstCompletion, cache->lastCompletion);
                        Rct::insertLinkedListNode(c, cache->firstCompletion, cache->lastCompletion, cache->lastCompletion);
                    }
                } else {
                    enum { MaxCompletionCache = 10 }; // ### configurable?
                    c = new Completion(request->location);
                    Rct::insertLinkedListNode(c, cache->firstCompletion, cache->lastCompletion, cache->lastCompletion);
                    while (cache->completionsMap.size() > MaxCompletionCache) {
                        Completion *cc = cache->firstCompletion;
                        Rct::removeLinkedListNode(cc, cache->firstCompletion, cache->lastCompletion);
                        delete cc;
                    }
                }
                c->completions.resize(nodeCount);
                for (int i=0; i<nodeCount; ++i)
                    c->completions[i] = nodes[i];
                printCompletions(c->completions, request);
                processTime = sw.elapsed();
                error("Processed %s, parse %d/%d, complete %d, process %d => %d completions (unsaved %d)",
                      sourceFile.constData(), parseTime, reparseTime, completeTime, processTime, nodeCount, request->unsaved.size());
            } else {
                error() << "Too many results available" << request->location << nodeCount;
            }
            clang_disposeCodeCompleteResults(results);
            delete[] nodes;
        } else {
            error() << "No completion results available" << request->location;
        }
    }
}

void CompletionThread::printCompletions(const List<Completion::Node> &completions, Request *request)
{
    static List<String> cursorKindNames;
    // error() << request->flags << testLog(RTags::CompilationErrorXml) << completions.size() << request->conn;
    const bool doLog = testLog(RTags::CompilationErrorXml);
    if (!(request->flags & Refresh) && (doLog || request->conn) && !completions.isEmpty()) {
        // Does this need to be called in the main thread?
        String out;
        out.reserve(16384);
        if (doLog)
            log(RTags::CompilationErrorXml, "<?xml version=\"1.0\" encoding=\"utf-8\"?><completions location=\"%s\"><![CDATA[",
                request->location.key().constData());

        if (request->flags & Elisp)
            out += "'(";
        for (const auto &val : completions) {
            if (val.cursorKind >= cursorKindNames.size())
                cursorKindNames.resize(val.cursorKind + 1);
            String &kind = cursorKindNames[val.cursorKind];
            if (kind.isEmpty())
                kind = RTags::eatString(clang_getCursorKindSpelling(val.cursorKind));
            if (request->flags & Elisp) {
                out += String::format<128>("\"%s\" \"%s\" \"%s\"",
                                           val.completion.constData(),
                                           val.signature.constData(),
                                           kind.constData());
            } else {
                out += String::format<128>("%s %s %s\n",
                                           val.completion.constData(),
                                           val.signature.constData(),
                                           kind.constData());
            }
        }
        if (request->flags & Elisp)
            out += ")";

        if (doLog) {
            logDirect(RTags::CompilationErrorXml, out);
            logDirect(RTags::CompilationErrorXml, "]]></completions>\n");
        }
        if (request->conn) {
            Connection *conn = request->conn;
            request->conn = 0;
            EventLoop::mainEventLoop()->callLater([conn, out]() {
                    // ### need to make sure this connection doesn't go away,
                    // probably need to disconnect something
                    conn->write(out); conn->finish();
                });
        }
    }
}
