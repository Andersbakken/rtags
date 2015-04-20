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

#include "RTagsLogOutput.h"
#include "CompletionThread.h"
#include "RTagsClang.h"
#include "Server.h"
#include "Token.h"

CompletionThread::CompletionThread(int cacheSize)
    : mShutdown(false), mCacheSize(cacheSize), mDump(0), mIndex(0)
{
}

CompletionThread::~CompletionThread()
{
    mCacheList.deleteAll();
}

void CompletionThread::run()
{
    mIndex = clang_createIndex(0, 1);
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
            for (SourceFile *cache = mCacheList.first(); cache; cache = cache->next) {
                out << cache->source << "\nhash:" << cache->unsavedHash
                    << "lastModified:" << cache->lastModified
                    << "translationUnit:" << cache->translationUnit << "\n";
                for (Completions *completion = cache->completionsList.first(); completion; completion = completion->next) {
                    out << "    " << completion->location.key() << "\n";
                    for (const auto &c : completion->candidates) {
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
                                  Flags<Flag> flags, const String &unsaved,
                                  const std::shared_ptr<Connection> &conn)
{
    Request *request = new Request({ source, location, flags, unsaved, conn});
    std::unique_lock<std::mutex> lock(mMutex);
    auto it = mPending.begin();
    while (it != mPending.end()) {
        if ((*it)->source == source) {
            delete *it;
            mPending.erase(it);
            break;
        }
        ++it;
    }
    mPending.push_front(request);
    mCondition.notify_one();
}

String CompletionThread::dump()
{
    Dump dump;
    dump.done = false;
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

int CompletionThread::compareCompletionCandidates(const void *left, const void *right)
{
    const Completions::Candidate *l = reinterpret_cast<const Completions::Candidate*>(left);
    const Completions::Candidate *r = reinterpret_cast<const Completions::Candidate*>(right);
    if (l->priority != r->priority)
        return l->priority < r->priority ? -1 : 1;
    if ((l->distance != -1) != (r->distance != -1))
        return l->distance != -1 ? -1 : 1;
    if (l->distance != r->distance)
        return l->distance > r->distance ? -1 : 1;
    return strcmp(l->completion.constData(), r->completion.constData());
}

#if 0
static inline const char *completionChunkKindSpelling(CXCompletionChunkKind kind)
{
    switch (kind) {
    case CXCompletionChunk_Optional: return "Optional";
    case CXCompletionChunk_TypedText: return "TypedText";
    case CXCompletionChunk_Text: return "Text";
    case CXCompletionChunk_Placeholder: return "Placeholder";
    case CXCompletionChunk_Informative: return "Informative";
    case CXCompletionChunk_CurrentParameter: return "CurrentParameter";
    case CXCompletionChunk_LeftParen: return "LeftParen";
    case CXCompletionChunk_RightParen: return "RightParen";
    case CXCompletionChunk_LeftBracket: return "LeftBracket";
    case CXCompletionChunk_RightBracket: return "RightBracket";
    case CXCompletionChunk_LeftBrace: return "LeftBrace";
    case CXCompletionChunk_RightBrace: return "RightBrace";
    case CXCompletionChunk_LeftAngle: return "LeftAngle";
    case CXCompletionChunk_RightAngle: return "RightAngle";
    case CXCompletionChunk_Comma: return "Comma";
    case CXCompletionChunk_ResultType: return "ResultType";
    case CXCompletionChunk_Colon: return "Colon";
    case CXCompletionChunk_SemiColon: return "SemiColon";
    case CXCompletionChunk_Equal: return "Equal";
    case CXCompletionChunk_HorizontalSpace: return "HorizontalSpace";
    case CXCompletionChunk_VerticalSpace: return "VerticalSpace";
    default: break;
    }
    return "";
}
#endif

void CompletionThread::process(Request *request)
{
    // if (!request->unsaved.isEmpty()) {
    //     int line = request->location.line();
    //     int pos = 0;
    //     while (line > 1) {
    //         int p = request->unsaved.indexOf('\n', pos);
    //         if (p == -1) {
    //             pos = -1;
    //             break;
    //         }
    //         pos = p + 1;
    //         --line;
    //     }
    //     if (pos != -1) {
    //         int end = request->unsaved.indexOf('\n', pos);
    //         if (end == -1)
    //             end = request->unsaved.size();
    //         error("Completing at %s:%d:%d line: [%s]\n",
    //               request->location.path().constData(),
    //               request->location.line(),
    //               request->location.column(),
    //               request->unsaved.mid(pos, end - pos).constData());
    //     }
    // }

    StopWatch sw;
    int parseTime = 0;
    int reparseTime = 0;
    int completeTime = 0;
    int processTime = 0;
    SourceFile *&cache = mCacheMap[request->source.fileId];
    if (cache && cache->source != request->source) {
        delete cache;
        cache = 0;
    }
    if (!cache) {
        cache = new SourceFile;
        mCacheList.append(cache);
        while (mCacheMap.size() > mCacheSize) {
            SourceFile *c = mCacheList.removeFirst();
            mCacheMap.remove(c->source.fileId);
            delete c;
        }
    } else {
        mCacheList.moveToEnd(cache);
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
        cache->completionsList.deleteAll();
        sw.restart();
        Flags<CXTranslationUnit_Flags> flags = static_cast<CXTranslationUnit_Flags>(clang_defaultEditingTranslationUnitOptions());
        flags |= CXTranslationUnit_PrecompiledPreamble;
        flags |= CXTranslationUnit_CacheCompletionResults;
        flags |= CXTranslationUnit_SkipFunctionBodies;
        // (CXTranslationUnit_PrecompiledPreamble
        // |CXTranslationUnit_CacheCompletionResults
        // |CXTranslationUnit_SkipFunctionBodies);

        const auto &options = Server::instance()->options();
        for (const auto &inc : options.includePaths) {
            request->source.includePaths << inc;
        }
        request->source.defines << options.defines;

        String clangLine;
        RTags::parseTranslationUnit(sourceFile, request->source.toCommandLine(Source::Default|Source::ExcludeDefaultArguments),
                                    cache->translationUnit, mIndex,
                                    &unsaved, request->unsaved.size() ? 1 : 0, flags, &clangLine);
        // error() << "PARSING" << clangLine;
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
        cache->completionsList.deleteAll();
        cache->unsavedHash = hash;
        cache->lastModified = lastModified;
    } else if (!(request->flags & Refresh)) {
        const auto it = cache->completionsMap.find(request->location);
        if (it != cache->completionsMap.end()) {
            cache->completionsList.moveToEnd(it->second);
            error("Found completions (%d) in cache %s:%d:%d",
                  it->second->candidates.size(), sourceFile.constData(),
                  request->location.line(), request->location.column());
            printCompletions(it->second->candidates, request);
            return;
        }
    }

    sw.restart();
    const unsigned int completionFlags = (CXCodeComplete_IncludeMacros|CXCodeComplete_IncludeCodePatterns);

    CXCodeCompleteResults *results = clang_codeCompleteAt(cache->translationUnit, sourceFile.constData(),
                                                          request->location.line(), request->location.column(),
                                                          &unsaved, unsaved.Length ? 1 : 0, completionFlags);
    completeTime = sw.restart();
    if (results) {
        Completions::Candidate *nodes = new Completions::Candidate[results->NumResults];
        int nodeCount = 0;
        Map<Token, int> tokens;
        if (!request->unsaved.isEmpty()) {
            tokens = Token::tokenize(request->unsaved.constData(), request->unsaved.size());
            // for (Map<Token, int>::const_iterator it = tokens.begin(); it != tokens.end(); ++it) {
            //     error() << String(it->first.data, it->first.length) << it->second;
            // }
        }
        for (unsigned int i = 0; i < results->NumResults; ++i) {
            const CXCursorKind kind = results->Results[i].CursorKind;
            if (kind == CXCursor_Destructor)
                continue;

            const CXCompletionString &string = results->Results[i].CompletionString;
            const CXAvailabilityKind availabilityKind = clang_getCompletionAvailability(string);
            if (availabilityKind != CXAvailability_Available)
                continue;

            const int priority = clang_getCompletionPriority(string);

            Completions::Candidate &node = nodes[nodeCount];
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
        if (nodeCount) {
            qsort(nodes, nodeCount, sizeof(Completions::Candidate), compareCompletionCandidates);
            Completions *&c = cache->completionsMap[request->location];
            if (c) {
                cache->completionsList.moveToEnd(c);
            } else {
                enum { MaxCompletionCache = 10 }; // ### configurable?
                c = new Completions(request->location);
                cache->completionsList.append(c);
                while (cache->completionsMap.size() > MaxCompletionCache) {
                    Completions *cc = cache->completionsList.takeFirst();
                    cache->completionsMap.remove(cc->location);
                    delete cc;
                }
            }
            c->candidates.resize(nodeCount);
            for (int i=0; i<nodeCount; ++i)
                c->candidates[i] = nodes[i];
            printCompletions(c->candidates, request);
            processTime = sw.elapsed();
            error("Processed %s, parse %d/%d, complete %d, process %d => %d completions (unsaved %d)",
                  sourceFile.constData(), parseTime, reparseTime, completeTime, processTime, nodeCount, request->unsaved.size());
            delete[] nodes;
        } else {
            printCompletions(List<Completions::Candidate>(), request);
            error() << "No completion results available" << request->location << results->NumResults;
        }
        clang_disposeCodeCompleteResults(results);
    }
}

struct Output
{
    void send(const String &string)
    {
        if (connection) {
            connection->write(string);
            connection->finish();
        } else {
            output->log(string.constData(), string.size());
        }
    }
    std::shared_ptr<LogOutput> output;
    std::shared_ptr<Connection> connection;
    bool xml;
};

void CompletionThread::printCompletions(const List<Completions::Candidate> &completions, Request *request)
{
    static List<String> cursorKindNames;
    // error() << request->flags << testLog(RTags::CompilationErrorXml) << completions.size() << request->conn;
    List<std::shared_ptr<Output> > outputs;
    bool xml = false;
    bool elisp = false;
    if (request->conn) {
        std::shared_ptr<Output> output(new Output);
        output->connection = request->conn;
        output->xml = !(request->flags & Elisp);
        outputs.append(output);
        if (request->flags & Elisp) {
            elisp = true;
        } else {
            xml = true;
        }
        request->conn.reset();
    }
    log([&xml, &elisp, &outputs](const std::shared_ptr<LogOutput> &output) {
            // error() << "Got a dude" << output->testLog(RTags::CompilationErrorXml);
            if (output->testLog(RTags::CompilationErrorXml)) {
                std::shared_ptr<Output> out(new Output);
                out->output = output;
                if (output->flags() & RTagsLogOutput::ElispList) {
                    out->xml = false;
                    elisp = true;
                } else {
                    out->xml = true;
                    xml = true;
                }
                outputs.append(out);
            }
        });

    if (!(request->flags & Refresh) && !outputs.isEmpty()) {
        String xmlOut, elispOut;
        if (xml) {
            xmlOut.reserve(16384);
            xmlOut << String::format<128>("<?xml version=\"1.0\" encoding=\"utf-8\"?><completions location=\"%s\"><![CDATA[",
                                          request->location.key().constData());
        }
        if (elisp) {
            elispOut.reserve(16384);
            elispOut += String::format<256>("(list 'completions (list \"%s\" (list", request->location.key().constData());
        }
        for (const auto &val : completions) {
            if (val.cursorKind >= cursorKindNames.size())
                cursorKindNames.resize(val.cursorKind + 1);
            String &kind = cursorKindNames[val.cursorKind];
            if (kind.isEmpty())
                kind = RTags::eatString(clang_getCursorKindSpelling(val.cursorKind));
            if (xml) {
                xmlOut += String::format<128>(" %s %s %s\n",
                                              val.completion.constData(),
                                              val.signature.constData(),
                                              kind.constData());
            }
            if (elisp) {
                elispOut += String::format<128>(" (list \"%s\" \"%s\" \"%s\")",
                                                val.completion.constData(),
                                                val.signature.constData(),
                                                kind.constData());
            }
        }
        if (elisp)
            elispOut += ")))";
        if (xml)
            xmlOut += "]]></completions>\n";

        EventLoop::mainEventLoop()->callLater([outputs, xmlOut, elispOut]() {
                for (auto &it : outputs) {
                    if (it->xml) {
                        it->send(xmlOut);
                    } else {
                        it->send(elispOut);
                    }
                }
            });
    }
}
