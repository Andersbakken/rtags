#include "CompletionThread.h"
#include "RTagsClang.h"
#include "Server.h"

CompletionThread::CompletionThread(int cacheSize)
    : mShutdown(false), mCacheSize(cacheSize), mIndex(0)
{
}

void CompletionThread::run()
{
    mIndex = clang_createIndex(0, 0);
    while (true) {
        Request *request = 0;
        {
            std::unique_lock<std::mutex> lock(mMutex);
            while (!mShutdown && mPending.isEmpty()) {
                mCondition.wait(lock);
            }
            if (mShutdown) {
                for (auto it = mPending.begin(); it != mPending.end(); ++it) {
                    delete *it;
                }
                mPending.clear();
                break;
            }
            assert(!mPending.isEmpty());
            request = mPending.takeFirst();
        }
        assert(request);
        process(request);
        delete request;
    }
    clang_disposeIndex(mIndex);
    mIndex = 0;
}

void CompletionThread::completeAt(const Source &source, const Location &location,
                                  unsigned int flags, const String &unsaved)
{
    Request *request = new Request({ source, location, flags, unsaved});
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

struct CompletionNode
{
    String completion, signature;
    int priority, distance;
};

static int compareCompletionNode(const void *left, const void *right)
{
    const CompletionNode *l = reinterpret_cast<const CompletionNode*>(left);
    const CompletionNode *r = reinterpret_cast<const CompletionNode*>(right);
    if (l->priority != r->priority)
        return l->priority < r->priority ? -1 : 1;
    if ((l->distance != -1) != (r->distance != -1))
        return l->distance != -1 ? -1 : 1;
    if (l->distance != r->distance)
        return l->distance > r->distance ? -1 : 1;
    return strcmp(l->completion.constData(), r->completion.constData());
}

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
};

static inline bool symbolChar(char ch)
{
    switch (ch) {
    case '_':
    case '~':
        return true;
    default:
        break;
    }
    return isalnum(ch);
}

static inline void addToken(const char *data, int pos, int len, Map<Token, int> &tokens)
{
    int &val = tokens[Token(data + pos, len)];
    if (!val)
        val = pos;
}

static inline void tokenize(const char *data, int size, Map<Token, int> &tokens)
{
    int tokenEnd = -1;
    for (int i=size - 1; i>=0; --i) {
        if (symbolChar(data[i])) {
            if (tokenEnd == -1)
                tokenEnd = i;
        } else if (tokenEnd != -1) {
            addToken(data, i + 1, tokenEnd - i, tokens);
            tokenEnd = -1;
        }
    }
    if (tokenEnd != -1)
        addToken(data, 0, tokenEnd + 1, tokens);
}

void CompletionThread::process(const Request *request)
{
    StopWatch sw;
    int parseTime = 0;
    int reparseTime = 0;
    int completeTime = 0;
    int processTime = 0;
    Cache *&cache = mCache[request->source.fileId];
    if (!cache)
        cache = new Cache({ 0, 0 });
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
    if (request->unsaved.size()) {
        std::hash<String> h;
        hash = h(request->unsaved);
    }

    if (!cache->translationUnit) {
        cache->completions.clear();
        cache->unsavedHash = hash;
        sw.restart();
        // ### maybe skip function bodies
        const unsigned int flags = clang_defaultEditingTranslationUnitOptions();
        // (CXTranslationUnit_PrecompiledPreamble
        // |CXTranslationUnit_CacheCompletionResults
        // |CXTranslationUnit_SkipFunctionBodies);
        RTags::parseTranslationUnit(sourceFile, request->source.arguments,
                                    List<String>(), // we don't want -fspell-checking and friends
                                    cache->translationUnit, mIndex,
                                    0, 0, flags);
                                    // &unsaved, unsaved.Length ? 1 : 0, flags);
        parseTime = sw.restart();
        if (cache->translationUnit) {
            RTags::reparseTranslationUnit(cache->translationUnit, 0, 0);
                                          // &unsaved,
                                          // unsaved.Length ? 1 : 0);

        }
        reparseTime = sw.elapsed();
        if (!cache->translationUnit)
            return;
    } else if (cache->unsavedHash != hash) {
        cache->completions.clear();
        cache->unsavedHash = hash;
    } else if (!(request->flags & Refresh)) {
        const auto it = cache->completions.find(request->location);
        if (it != cache->completions.end()) {
            error("Found completions (%d) in cache %s:%d:%d",
                  it->second.size(), sourceFile.constData(),
                  request->location.line(), request->location.column());
            printCompletions(it->second, request);
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
        CompletionNode *nodes = new CompletionNode[results->NumResults];
        int nodeCount = 0;
        Map<Token, int> tokens;
        if (!request->unsaved.isEmpty()) {
            tokenize(request->unsaved.constData(), request->unsaved.size(), tokens);
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
            if (priority >= 75)
                continue;

            CompletionNode &node = nodes[nodeCount];
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
        if (nodeCount) {
            enum { SendThreshold = 500 };
            if (nodeCount <= SendThreshold) {
                qsort(nodes, nodeCount, sizeof(CompletionNode), compareCompletionNode);
                List<std::pair<String, String> > &completions = cache->completions[request->location];
                completions.reserve(nodeCount);
                for (int i=0; i<nodeCount; ++i)
                    completions.append(std::make_pair(nodes[i].completion, nodes[i].signature));
                printCompletions(completions, request);
            }
        }

        delete[] nodes;
        clang_disposeCodeCompleteResults(results);
        processTime = sw.elapsed();
        error("Processed %s, parse %d/%d, complete %d, process %d => %d completions (unsaved %d)",
              sourceFile.constData(), parseTime, reparseTime, completeTime, processTime, nodeCount, request->unsaved.size());
    } else {
        error() << "No completion results available" << request->location;
    }
}

void CompletionThread::printCompletions(const List<std::pair<String, String> > &completions, const Request *request)
{
    error() << request->flags << testLog(RTags::CompilationErrorXml) << completions.size();
    if (!(request->flags & Refresh) && testLog(RTags::CompilationErrorXml) && !completions.isEmpty()) {
        // Does this need to be called in the main thread?
        log(RTags::CompilationErrorXml, "<completions location=\"%s\">![CDATA[",
            request->location.key().constData());

        if (request->flags & Elisp)
            logDirect(RTags::CompilationErrorXml, "'(");
        for (auto it = completions.begin(); it != completions.end(); ++it) {
            const std::pair<String, String> &val = *it;
            if (request->flags & Elisp) {
                log(RTags::CompilationErrorXml, "\"%s\" \"%s\"", val.first.constData(), val.second.constData());
            } else {
                log(RTags::CompilationErrorXml, "%s %s\n", val.first.constData(), val.second.constData());
            }
        }
        if (request->flags & Elisp) {
            logDirect(RTags::CompilationErrorXml, ")]]</completions>\n");
        } else {
            logDirect(RTags::CompilationErrorXml, "]]</completions>\n");
        }
    }
}
