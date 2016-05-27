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

#include "CompletionThread.h"

#include "rct/StopWatch.h"
#include "RTags.h"
#include "RTagsLogOutput.h"
#include "Server.h"

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
                    std::unique_lock<std::mutex> dumpLock(mDump->mutex);
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
                    << "\nlastModified:" << cache->lastModified
                    << "\nparseTime:" << cache->parseTime
                    << "\nreparseTime:" << cache->reparseTime
                    << "\ncompletions:" << cache->completions
                    << "\ncompletionTime:" << cache->codeCompleteTime
                    << (cache->completions
                        ? String::format<32>("(avg: %.2f)",
                                             (static_cast<double>(cache->codeCompleteTime) / cache->completions))
                        : String())
                    << "\ntranslationUnit:" << cache->translationUnit << "\n";
                for (Completions *completion = cache->completionsList.first(); completion; completion = completion->next) {
                    out << "    " << completion->location.toString() << "\n";
                    for (const auto &c : completion->candidates) {
                        out << "        " << ('"' + c.completion + '"') << ('+' + c.signature + '"') << c.priority << c.distance
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

void CompletionThread::completeAt(const Source &source, Location location,
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

bool CompletionThread::compareCompletionCandidates(const Completions::Candidate *l,
                                                   const Completions::Candidate *r)
{
    if (l->priority != r->priority)
        return l->priority < r->priority;
    if ((l->distance != -1) != (r->distance != -1))
        return l->distance != -1;
    if (l->distance != r->distance)
        return l->distance > r->distance;
    return l->completion < r->completion;
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
    const bool sendDebug = testLog(LogLevel::Debug);

    assert(!cache->translationUnit || cache->source == request->source);
    if (!cache->translationUnit) {
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

    const auto &options = Server::instance()->options();
    if (!cache->translationUnit) {
        cache->completionsMap.clear();
        cache->completionsList.deleteAll();
        sw.restart();
        Flags<CXTranslationUnit_Flags> flags = static_cast<CXTranslationUnit_Flags>(clang_defaultEditingTranslationUnitOptions());
        flags |= CXTranslationUnit_PrecompiledPreamble;
        flags |= CXTranslationUnit_CacheCompletionResults;
        flags |= CXTranslationUnit_SkipFunctionBodies;
        flags |= CXTranslationUnit_DetailedPreprocessingRecord;
        flags |= CXTranslationUnit_Incomplete;
        flags |= CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;

        for (const auto &inc : options.includePaths) {
            request->source.includePaths << inc;
        }
        request->source.defines << options.defines;

        cache->translationUnit = RTags::TranslationUnit::create(sourceFile,
                                                                request->source.toCommandLine(Source::Default|Source::ExcludeDefaultArguments),
                                                                &unsaved, request->unsaved.size() ? 1 : 0, flags);
        // error() << "PARSING" << clangLine;
        parseTime = cache->parseTime = sw.restart();
        if (cache->translationUnit) {
            cache->translationUnit->reparse(&unsaved, request->unsaved.size() ? 1 : 0);
        }
        reparseTime = cache->reparseTime = sw.elapsed();
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
            error("Found completions (%zu) in cache %s:%d:%d",
                  it->second->candidates.size(), sourceFile.constData(),
                  request->location.line(), request->location.column());
            printCompletions(it->second->candidates, request);
            return;
        }
    }

    sw.restart();
    unsigned int completionFlags = (CXCodeComplete_IncludeCodePatterns|CXCodeComplete_IncludeBriefComments);
    if (request->flags & CodeCompleteIncludeMacros)
        completionFlags |= CXCodeComplete_IncludeMacros;

    CXCodeCompleteResults *results = clang_codeCompleteAt(cache->translationUnit->unit, sourceFile.constData(),
                                                          request->location.line(), request->location.column(),
                                                          &unsaved, unsaved.Length ? 1 : 0, completionFlags);
    completeTime = cache->codeCompleteTime = sw.restart();
    ++cache->completions;
    if (results) {
        std::vector<Completions::Candidate> nodes;
        nodes.reserve(results->NumResults);

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
            if (!(options.options & Server::CompletionsNoFilter) && kind == CXCursor_Destructor)
                continue;

            const CXCompletionString &string = results->Results[i].CompletionString;

            const CXAvailabilityKind availabilityKind = clang_getCompletionAvailability(string);
            if (!(options.options & Server::CompletionsNoFilter)) {
                switch (availabilityKind) {
                case CXAvailability_Available:
                    break;
                case CXAvailability_Deprecated:
                    break;
                case CXAvailability_NotAccessible:
                    break;
                case CXAvailability_NotAvailable: // protected members are erroneously flagged as NotAvailable in clang 3.6
                    continue;
                }
            }

            const int priority = clang_getCompletionPriority(string);

            if (static_cast<size_t>(nodeCount) == nodes.size())
                nodes.emplace_back();

            Completions::Candidate &node = nodes.back();
            node.cursorKind = kind;
            node.priority = priority;
            node.signature.reserve(256);
            const int chunkCount = clang_getNumCompletionChunks(string);
            bool ok = true;
            for (int j=0; j<chunkCount; ++j) {
                const CXCompletionChunkKind chunkKind = clang_getCompletionChunkKind(string, j);
                if (chunkKind == CXCompletionChunk_TypedText) {
                    node.completion = RTags::eatString(clang_getCompletionChunkText(string, j));
                    if (node.completion.isEmpty()
                        || (node.completion.size() > 8
                            && node.completion.startsWith("operator")
                            && !isPartOfSymbol(node.completion.at(8)))) {
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
                const unsigned int annotations = clang_getCompletionNumAnnotations(string);
                for (unsigned i=0; i<annotations; ++i) {
                    const CXStringScope annotation = clang_getCompletionAnnotation(string, i);
                    const char *cstr = clang_getCString(annotation);
                    if (const int len = strlen(cstr)) {
                        if (!node.annotation.isEmpty())
                            node.annotation.append(' ');
                        node.annotation.append(cstr, len);
                    }
                }
                node.parent = RTags::eatString(clang_getCompletionParent(string, 0));
                node.briefComment = RTags::eatString(clang_getCompletionBriefComment(string));

                int ws = node.completion.size() - 1;
                while (ws >= 0 && isspace(node.completion.at(ws)))
                    --ws;
                if (ws >= 0) {
                    node.completion.truncate(ws + 1);
                    node.signature.replace("\n", "");
                    node.distance = tokens.isEmpty() ? -1 : tokens.value(Token(node.completion.constData(), node.completion.size()), -1);
                    if (sendDebug)
                        debug() << node.signature << node.priority << kind
                                << node.distance << clang_getCompletionAvailability(string);

                    ++nodeCount;
                    continue;
                }
            }
            node.completion.clear();
            node.signature.clear();
        }
        if (nodeCount) {
            // Sort pointers instead of shuffling candidates around
            std::vector<const Completions::Candidate*> nodesPtr;
            nodesPtr.reserve(nodeCount);
            for (const auto &n : nodes)
                nodesPtr.push_back(&n);

            std::sort(nodesPtr.begin(), nodesPtr.end(), compareCompletionCandidates);

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
                c->candidates[i] = std::move(*nodesPtr[i]);
            printCompletions(c->candidates, request);
            processTime = sw.elapsed();
            warning("Processed %s, parse %d/%d, complete %d, process %d => %d completions (unsaved %zu)%s",
                    request->location.toString().constData(),
                    parseTime, reparseTime, completeTime, processTime, nodeCount, request->unsaved.size(),
                    request->flags & Refresh ? " Refresh" : "");
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
    Flags<CompletionThread::Flag> flags;
};

void CompletionThread::printCompletions(const List<Completions::Candidate> &completions, Request *request)
{
    if (request->flags & Refresh)
        return;
    static List<String> cursorKindNames;
    // error() << request->flags << testLog(RTags::DiagnosticsLevel) << completions.size() << request->conn;
    List<std::shared_ptr<Output> > outputs;
    bool xml = false;
    bool elisp = false;
    bool raw = false;
    bool json = false;
    if (request->conn) {
        std::shared_ptr<Output> output(new Output);
        output->connection = request->conn;
        output->flags = request->flags;
        outputs.append(output);
        if (request->flags & Elisp) {
            elisp = true;
        } else if (request->flags & XML) {
            xml = true;
        } else if (request->flags & JSON) {
            json = true;
        } else {
            raw = true;
        }
        request->conn.reset();
    }
    log([&xml, &elisp, &outputs, &raw, &json](const std::shared_ptr<LogOutput> &output) {
            // error() << "Got a dude" << output->testLog(RTags::DiagnosticsLevel);
            if (output->testLog(RTags::DiagnosticsLevel)) {
                std::shared_ptr<Output> out(new Output);
                out->output = output;
                if (output->flags() & RTagsLogOutput::Elisp) {
                    out->flags |= CompletionThread::Elisp;
                    elisp = true;
                } else if (output->flags() & RTagsLogOutput::XML) {
                    out->flags |= CompletionThread::XML;
                    xml = true;
                } else if (output->flags() & RTagsLogOutput::JSON) {
                    out->flags |= CompletionThread::JSON;
                    json = true;
                } else {
                    raw = true;
                }
                outputs.append(out);
            }
        });

    if (!outputs.isEmpty()) {
        String rawOut, xmlOut, jsonOut, elispOut;
        if (raw)
            rawOut.reserve(16384);
        if (xml) {
            xmlOut.reserve(16384);
            xmlOut += String::format<128>("<?xml version=\"1.0\" encoding=\"utf-8\"?><completions location=\"%s\"><![CDATA[",
                                          request->location.toString(Location::AbsolutePath).constData());
        }
        if (elisp) {
            elispOut.reserve(16384);
            elispOut += String::format<256>("(list 'completions (list \"%s\" (list",
                                            RTags::elispEscape(request->location.toString(Location::AbsolutePath)).constData());
        }
        for (const auto &val : completions) {
            if (val.cursorKind >= cursorKindNames.size())
                cursorKindNames.resize(val.cursorKind + 1);
            String &kind = cursorKindNames[val.cursorKind];
            if (kind.isEmpty())
                kind = RTags::eatString(clang_getCursorKindSpelling(val.cursorKind));
            if (xml || raw) {
                const String str = String::format<128>(" %s %s %s %s %s %s\n",
                                                       val.completion.constData(),
                                                       val.signature.constData(),
                                                       kind.constData(),
                                                       val.annotation.constData(),
                                                       val.parent.constData(),
                                                       val.briefComment.constData());
                if (raw)
                    rawOut += str;
                if (xml)
                    xmlOut += str;
            }
            if (elisp) {
                // elispOut += String::format<128>(" (list \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\")",
                elispOut += String::format<128>(" (list \"%s\" \"%s\" \"%s\")",
                                                RTags::elispEscape(val.completion).constData(),
                                                RTags::elispEscape(val.signature).constData(),
                                                kind.constData());
                //,
                // RTags::elispEscape(val.annotation).constData(),
                // val.parent.constData(),
                // val.briefComment.constData());
            }
            if (json) {
                if (jsonOut.isEmpty()) {
                    jsonOut.reserve(16384);
                    jsonOut += "{\"completions\":[";
                } else {
                    jsonOut += ',';
                }

                jsonOut += String::format<1024>("{\"completion\":%s,\"signature\":%s,\"kind\":\"%s\"}",
                                                Rct::jsonEscape(val.completion).constData(),
                                                Rct::jsonEscape(val.signature).constData(),
                                                kind.constData());
                                                          }
            }
            if (elisp)
                elispOut += ")))";
            if (xml)
                xmlOut += "]]></completions>\n";
            if (json)
                jsonOut += "]}";

            EventLoop::mainEventLoop()->callLater([outputs, xmlOut, elispOut, rawOut, jsonOut]() {
                    for (auto &it : outputs) {
                        if (it->flags & Elisp) {
                            it->send(elispOut);
                        } else if (it->flags & XML) {
                            it->send(xmlOut);
                        } else if (it->flags & JSON) {
                            it->send(jsonOut);
                        } else {
                            it->send(rawOut);
                        }
                    }
                });
        }
    }
