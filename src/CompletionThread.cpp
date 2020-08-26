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

#include "CompletionThread.h"

#include <assert.h>
#include <string.h>
#include <functional>
#include <list>
#include <unordered_map>

#include "IndexDataMessage.h"
#include "Project.h"
#include "rct/StopWatch.h"
#include "RTags.h"
#include "RTagsLogOutput.h"
#include "Server.h"
#include "rct/StringTokenizer.h"
#include "Diagnostic.h"
#include "QueryMessage.h"
#include "clang-c/CXString.h"
#include "clang-c/Index.h"
#include "rct/EventLoop.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Rct.h"
#ifdef HAS_JSON_H
#include "rct/json/json.hpp"

using namespace nlohmann;
#endif

static uint64_t start = 0;
#define LOG()                                                           \
    if (Server::instance()->options().options & Server::CompletionLogs) \
        error() << "CODE COMPLETION"                                    \
                << String::format<64>("%gs %s",                         \
                                      static_cast<double>(Rct::monoMs() - ::start) / 1000.0, \
                                      Rct::currentTimeString().constData())

CompletionThread::CompletionThread(int cacheSize)
    : mShutdown(false), mCacheSize(cacheSize), mDump(nullptr)
{
}

CompletionThread::~CompletionThread()
{
    mCacheList.deleteAll();
}

void CompletionThread::run()
{
    while (true) {
        Request *request = nullptr;
        Dump *dump = nullptr;
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
                    mDump = nullptr;
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
                out << cache->source
                    << "\nparseTime:" << cache->parseTime
                    << "\nreparseTime:" << cache->reparseTime
                    << "\ncompletions:" << cache->completions
                    << "\ncompletionTime:" << cache->codeCompleteTime
                    << (cache->completions
                        ? String::format<32>("(avg: %.2f)",
                                             (static_cast<double>(cache->codeCompleteTime) / cache->completions))
                        : String())
                    << "\ntranslationUnit:" << cache->translationUnit << "\n";
            }
            dump->done = true;
            dump->cond.notify_one();
        } else {
            assert(request);
            process(request);
            delete request;
        }
    }
}

void CompletionThread::completeAt(Source &&source, Location location,
                                  Flags<Flag> flags, int max, const UnsavedFiles &unsavedFiles,
                                  const String &prefix,
                                  const std::shared_ptr<Connection> &conn)
{
    if (Server::instance()->options().options & Server::CompletionLogs)
        error() << "CODE COMPLETION completeAt" << Rct::currentTimeString() << location << flags;

    Request *request = new Request({ std::forward<Source>(source), location, flags, max, unsavedFiles, prefix, conn});
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

void CompletionThread::prepare(Source &&source, const UnsavedFiles &unsavedFiles)
{
    if (Server::instance()->options().options & Server::CompletionLogs) {
        const auto it = unsavedFiles.find(source.sourceFile());
        const auto unsavedSize = (it != unsavedFiles.end()) ? it->second.size() : 0u;
        error() << "CODE COMPLETION prepare" << Rct::currentTimeString() << source.sourceFile() << unsavedSize;
    }
    std::unique_lock<std::mutex> lock(mMutex);
    for (auto req : mPending) {
        if (req->source == source) {
            req->unsavedFiles = unsavedFiles;
            return;
        }
    }

    Request *request = new Request({ std::forward<Source>(source), Location(), WarmUp, -1, unsavedFiles, String(), std::shared_ptr<Connection>() });
    mPending.push_back(request);
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

bool CompletionThread::compareCompletionCandidates(const Completions::Candidate *l,
                                                   const Completions::Candidate *r)
{
    if (l->priority != r->priority)
        return l->priority < r->priority;
#ifdef RTAGS_COMPLETION_TOKENS_ENABLED
    if ((l->distance != -1) != (r->distance != -1))
        return l->distance != -1;
    if (l->distance != r->distance)
        return l->distance > r->distance;
#endif
    return l->completion < r->completion;
}

void CompletionThread::process(Request *request)
{
    ::start = Rct::monoMs();
    LOG() << "processing" << request->toString();
    StopWatch sw;
    int parseTime = 0;
    int reparseTime = 0;
    int completeTime = 0;
    int processTime = 0;
    mMutex.lock();
    SourceFile *&cache = mCacheMap[request->source.fileId];

    if (cache && cache->source != request->source) {
        LOG() << "cached sourcefile doesn't match source, discarding" << request->source.sourceFile()
              << cache->source << "vs" << request->source;
        mCacheList.remove(cache);
        delete cache;
        cache = nullptr;
    }
    if (!cache) {
        cache = new SourceFile;
        LOG() << "creating source file for" << request->source.sourceFile();
        mCacheList.append(cache);
        while (mCacheMap.size() > mCacheSize) {
            SourceFile *c = mCacheList.removeFirst();
            LOG() << "over cache limit. discarding" << c->source.sourceFile();
            mCacheMap.remove(c->source.fileId);
            delete c;
        }
    } else {
        mCacheList.moveToEnd(cache);
    }
    mMutex.unlock();

    assert(!cache->translationUnit || cache->source == request->source);
    if (!cache->translationUnit) {
        cache->source = std::move(request->source);
        assert(!cache->source.defines.contains(Source::Define("RTAGS", String(), Source::Define::NoValue)));
    }

    const Path sourceFile = cache->source.sourceFile();
    List<CXUnsavedFile> unsavedFiles;
    unsavedFiles.reserve(request->unsavedFiles.size());
    for (const auto &it : request->unsavedFiles) {
        unsavedFiles.push_back({ it.first.constData(), it.second.constData(), static_cast<unsigned long>(it.second.size()) });
    }

    const auto &options = Server::instance()->options();
    bool reparse = false;
    if (!cache->translationUnit) {
        if (request->conn && request->flags & NoWait) {
            request->flags |= WarmUp;
            if (request->flags & Elisp) {
                request->conn->finish("(list (cons 'pending t))");
            } else if (request->flags & XML) {
                request->conn->finish("<?xml version=\"1.0\" encoding=\"utf-8\"?><completions pending=\"true\">");
            } else if (request->flags & JSON) {
                request->conn->finish("{\"pending\":true}");
            } else {
                request->conn->finish("pending");
            }
            request->conn.reset();
        }
        LOG() << "No translationUnit for" << cache->source.sourceFile() << "recreating";
        sw.restart();
        Flags<CXTranslationUnit_Flags> flags = static_cast<CXTranslationUnit_Flags>(clang_defaultEditingTranslationUnitOptions());
        flags |= CXTranslationUnit_CacheCompletionResults;
        flags |= CXTranslationUnit_DetailedPreprocessingRecord;
        flags |= CXTranslationUnit_Incomplete;
        flags |= CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
        flags |= CXTranslationUnit_PrecompiledPreamble;
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 32)
        flags |= CXTranslationUnit_CreatePreambleOnFirstParse;
#endif

        cache->translationUnit = RTags::TranslationUnit::create(sourceFile,
                                                                cache->source.toCommandLine(Source::Default|Source::ExcludeDefaultArguments),
                                                                unsavedFiles.data(), static_cast<int>(unsavedFiles.size()), flags, false);
        // error() << "PARSING" << clangLine;
        parseTime = cache->parseTime = sw.elapsed();
        // with clang 3.8 it definitely seems like we have to reparse once to
        // generate the preamble. Even with CXTranslationUnit_CreatePreambleOnFirstParse
        if (!cache->translationUnit) {
            LOG() << "Failed to parse translation unit" << cache->source.sourceFile();
            return;
        }
        reparse = true;
    } else if (!request->unsavedFiles.isEmpty()) {
        reparse = request->unsavedFiles != cache->unsavedFiles;
        cache->lastModified = 0;
    } else {
        const uint64_t lastModified = cache->source.sourceFile().lastModifiedMs();
        if (lastModified != cache->lastModified) {
            cache->lastModified = lastModified;
            cache->unsavedFiles.clear();
            reparse = true;
        } else {
            assert(cache->unsavedFiles.isEmpty());
        }
    }

    LOG() << "line: " << request->location.line() << "column: "
          << request->location.column() << ", prefix: "
          << request->prefix;

    if (reparse) {
        sw.restart();
        assert(cache->translationUnit);
        LOG() << "reparsing translation unit" << cache->source.sourceFile();
        cache->translationUnit->reparse(unsavedFiles.data(), static_cast<int>(unsavedFiles.size()));
        reparseTime = cache->reparseTime = sw.elapsed();
        cache->unsavedFiles = std::move(request->unsavedFiles);
    }


    if (request->flags & WarmUp) {
        LOG() << "Warmed up unit" << cache->source.sourceFile();
        return;
    } else if (request->flags & Diagnose) {
        processDiagnostics(request, nullptr, cache->translationUnit->unit);
        return;
    }

    sw.restart();
    unsigned int completionFlags = (CXCodeComplete_IncludeCodePatterns|CXCodeComplete_IncludeBriefComments);
    if (request->flags & IncludeMacros)
        completionFlags |= CXCodeComplete_IncludeMacros;

    CXCodeCompleteResults *results = clang_codeCompleteAt(cache->translationUnit->unit, request->location.path().c_str(),
                                                          request->location.line(), request->location.column(),
                                                          unsavedFiles.data(), static_cast<unsigned int>(unsavedFiles.size()), completionFlags);
    completeTime = cache->codeCompleteTime = sw.restart();
    LOG() << "Generated" << (results ? results->NumResults : 0) << "completions for" << request->location << "from" << sourceFile << (results ? "successfully" : "unsuccessfully") << "in" << completeTime << "ms";

    ++cache->completions;
    if (results) {
        List<CompletionCandidate *> candidates;
        candidates.reserve(results->NumResults);

        const auto it = cache->unsavedFiles.find(cache->source.sourceFile());
        const String *unsaved = (it != cache->unsavedFiles.end()) ? &it->second : nullptr;

        int nodeCount = 0;
#ifdef RTAGS_COMPLETION_TOKENS_ENABLED
        Map<Token, int> tokens;
        if (unsaved) {
            tokens = Token::tokenize(unsaved->constData(), static_cast<int>(unsaved->size()));
            // for (Map<Token, int>::const_iterator it = tokens.begin(); it != tokens.end(); ++it) {
            //     error() << String(it->first.data, it->first.length) << it->second;
            // }
        }
#endif
        for (unsigned int i = 0; i < results->NumResults; ++i) {
            const CXCursorKind kind = results->Results[i].CursorKind;
            const CXCompletionString &string = results->Results[i].CompletionString;

            const CXAvailabilityKind availabilityKind = clang_getCompletionAvailability(string);
            if (!(options.options & Server::CompletionsNoFilter)) {
                switch (availabilityKind) {
                case CXAvailability_Available:
                    break;
                case CXAvailability_Deprecated:
                    break;
                case CXAvailability_NotAccessible:
                    continue;
                case CXAvailability_NotAvailable: // protected members are erroneously flagged as NotAvailable in clang 3.6
                    continue;
                }
            }

            const int priority = clang_getCompletionPriority(string);

            CompletionCandidate *candidate = new CompletionCandidate;

            const int chunkCount = clang_getNumCompletionChunks(string);
            for (int j=0; j<chunkCount; ++j) {
                const CXCompletionChunkKind chunkKind = clang_getCompletionChunkKind(string, j);
                String text = RTags::eatString(clang_getCompletionChunkText(string, j));
                if (chunkKind == CXCompletionChunk_TypedText) {
                    candidate->name = text;
                    if (candidate->name.isEmpty() || (candidate->name == "RTAGS" && kind == CXCursor_MacroDefinition)) {
                        delete candidate;
                        candidate = nullptr;
                        break;
                    }
                    candidate->signature += candidate->name;
                } else {
                    candidate->signature += text;
                    if (chunkKind == CXCompletionChunk_ResultType)
                        candidate->signature += ' ';
                }
            }
            if (candidate) {
                candidate->kind = RTags::eatString(clang_getCursorKindSpelling(kind));
                candidate->priority = priority;
                candidate->parent = RTags::eatString(clang_getCompletionParent(string, nullptr));
                candidate->brief_comment = RTags::eatString(clang_getCompletionBriefComment(string));
                candidates.push_back(candidate);
                const unsigned int annotations = clang_getCompletionNumAnnotations(string);
                for (unsigned j=0; j<annotations; ++j) {
                    const CXStringScope annotation = clang_getCompletionAnnotation(string, j);
                    const char *cstr = clang_getCString(annotation);
                    if (strlen(cstr)) {
                        if (!candidate->annotation.isEmpty())
                            candidate->annotation += ' ';
                        candidate->annotation + cstr;
                    }
                }
            }
        }

        List<std::unique_ptr<MatchResult> > matches = StringTokenizer::find_and_sort_matches(candidates, request->prefix);

        if ((request->max != -1) && (static_cast<size_t>(request->max) < matches.size())) {
            matches.resize(request->max);
        }

        if (!matches.isEmpty()) {
            printCompletions(matches, request);
            processTime = sw.elapsed();
            LOG() << "Sent" << matches.size() << "completions for" << request->location;
            warning("Processed %s, parse %d/%d, complete %d, process %d => %d completions (unsaved %zu)",
                    request->location.toString().constData(),
                    parseTime, reparseTime, completeTime, processTime, nodeCount, unsaved ? unsaved->size() : 0);

        } else {
            LOG() << "No completions available for" << request->location;
            printCompletions(List<std::unique_ptr<MatchResult> >(), request);
        }

        if (options.options & Server::CompletionDiagnostics)
            processDiagnostics(request, results, cache->translationUnit->unit);
        clang_disposeCodeCompleteResults(results);
    }
}

Value CompletionThread::Completions::Candidate::toValue(unsigned int f) const
{
    Value ret;
    if (!completion.isEmpty())
        ret["completion"] = completion;
    if (!signature.isEmpty())
        ret["signature"] = signature;
    if (!annotation.isEmpty())
        ret["annotation"] = annotation;
    if (!parent.isEmpty())
        ret["parent"] = parent;
    if (!briefComment.isEmpty())
        ret["briefComment"] = briefComment;
    ret["priority"] = priority;
#ifdef RTAGS_COMPLETION_TOKENS_ENABLED
    ret["distance"] = distance;
#endif
    String str;
    str << cursorKind;
    ret["kind"] = str;
    if (f & Flag_IncludeChunks && !chunks.isEmpty()) {
        Value cc;
        cc.arrayReserve(chunks.size());
        for (const auto &chunk : chunks) {
            Value c;
            c["text"] = chunk.text;
            String kind;
            kind << chunk.kind;
            c["kind"] = kind;
            cc.push_back(c);
        }
        ret["chunks"] = cc;
    }
    return ret;
}

struct Output
{
    void send(const String &string)
    {
        if (connection) {
            connection->write(string);
            connection->finish();
        } else {
            output->log(string.constData());
        }
    }
    std::shared_ptr<LogOutput> output;
    std::shared_ptr<Connection> connection;
    Flags<CompletionThread::Flag> flags;
};

void CompletionThread::printCompletions(const List<std::unique_ptr<MatchResult> > &results, Request *request)
{
    static List<String> cursorKindNames;
    // error() << request->flags << testLog(RTags::DiagnosticsLevel) << completions.size() << request->conn;
    List<std::shared_ptr<Output> > outputs;
    bool xml = false;
    bool elisp = false;
    bool raw = false;
#ifdef HAS_JSON_H
    bool send_json = false;
    json j = {{"completions", json::array()}};
#endif
    if (request->conn) {
        auto output = std::make_shared<Output>();
        output->connection = request->conn;
        output->flags = request->flags;
        outputs.append(output);
        if (request->flags & Elisp) {
            elisp = true;
        } else if (request->flags & XML) {
            xml = true;
#ifdef HAS_JSON_H
        } else if (request->flags & JSON) {
            send_json = true;
#endif
        } else {
            raw = true;
        }
        request->conn.reset();
    } else {
        log([&](const std::shared_ptr<LogOutput> &output) {
                // error() << "Got a dude" << output->testLog(RTags::DiagnosticsLevel);
                if (output->testLog(RTags::DiagnosticsLevel)) {
                    auto out = std::make_shared<Output>();
                    out->output = output;
                    if (output->type() == LogOutput::Custom) {
                        const Flags<QueryMessage::Flag> queryFlags = std::static_pointer_cast<RTagsLogOutput>(output)->queryFlags();
                        if (queryFlags & QueryMessage::Elisp) {
                            out->flags |= CompletionThread::Elisp;
                            elisp = true;
                        } else if (queryFlags & QueryMessage::XML) {
                            out->flags |= CompletionThread::XML;
                            xml = true;
#ifdef HAS_JSON_H
                        } else if (queryFlags & QueryMessage::JSON) {
                            out->flags |= CompletionThread::JSON;
                            send_json = true;
#endif
                        }
                    } else {
                        raw = true;
                    }
                    outputs.append(out);
                }
            });
    }

    if (!outputs.isEmpty()) {
        String rawOut, xmlOut, elispOut;
#ifdef HAS_JSON_H
        String jsonOut;
#endif
        if (raw)
            rawOut.reserve(16384);
        if (xml) {
            xmlOut.reserve(16384);
            xmlOut += String::format<128>("<?xml version=\"1.0\" encoding=\"utf-8\"?><completions location=\"%s\"><![CDATA[",
                                          request->location.toString(Location::AbsolutePath).constData());
        }
        if (elisp) {
            elispOut.reserve(16384);
            elispOut << String::format<256>("(list 'completions (list \"%s\" (list",
                                            RTags::elispEscape(request->location.toString(Location::AbsolutePath)).constData());
        }
        for (const std::unique_ptr<MatchResult> &result : results) {
            CompletionCandidate *c = result->candidate;
            const String str = String::format<128>(" %s %s %s %s %s %s\n",
                                                   c->name.c_str(),
                                                   c->signature.c_str(),
                                                   c->kind.c_str(),
                                                   c->annotation.c_str(),
                                                   c->parent.c_str(),
                                                   c->brief_comment.c_str());


            if (xml || raw)
                if (raw)
                    rawOut += str;
            if (xml)
                xmlOut += str;
#ifdef HAS_JSON_H
            if (send_json) {
                j["completions"] += {
                    {"completion", c->name},
                    {"signature", c->signature},
                    {"kind", c->kind},
                    {"parent", c->parent},
                    {"brief_comment", c->brief_comment},
                    {"annotation", c->annotation},
                    {"priority", c->priority}
                };
            }
#endif
            if (elisp) {
                elispOut += String::format<128>(" (list \"%s\" \"%s\" \"%s\" \"%s\")",
                                                RTags::elispEscape(c->name).constData(),
                                                RTags::elispEscape(c->signature).constData(),
                                                c->kind.c_str(),
                                                RTags::elispEscape(c->brief_comment).constData());
                //,
                // RTags::elispEscape(val->annotation).constData(),
                // val->parent.constData(),
                // val->briefComment.constData());
            }
        }
        if (elisp)
            elispOut += ")))";
        if (xml)
            xmlOut += "]]></completions>\n";
#ifdef HAS_JSON_H
        if (send_json)
            jsonOut = j.dump(4);
#endif

        EventLoop::mainEventLoop()->callLater([outputs, xmlOut, elispOut, rawOut
#ifdef HAS_JSON_H
                                               , jsonOut
#endif
                                                  ]() {
                                                  for (auto &it : outputs) {
                                                      if (it->flags & Elisp) {
                                                          it->send(elispOut);
                                                      } else if (it->flags & XML) {
                                                          it->send(xmlOut);
#ifdef HAS_JSON_H
                                                      } else if (it->flags & JSON) {
                                                          it->send(jsonOut);
#endif
                                                      } else {
                                                          it->send(rawOut);
                                                      }
                                                  }
                                              });
    }
}

bool CompletionThread::isCached(const std::shared_ptr<Project> &project, uint32_t fileId) const
{
    std::unique_lock<std::mutex> lock(mMutex);
    for (SourceFile *file : mCacheList) {
        if (file->source.fileId == fileId || project->dependsOn(file->source.fileId, fileId))
            return true;
    }
    return false;
}

void CompletionThread::reparse(const std::shared_ptr<Project> &/*project*/, uint32_t fileId)
{
    if (!(Server::instance()->options().options & Server::CompletionDiagnostics))
        return;
    std::unique_lock<std::mutex> lock(mMutex);
    Source source;
    for (SourceFile *file : mCacheList) {
        if (file->source.fileId == fileId) { // || project->dependsOn(file->source.fileId, fileId)) {
            source = file->source;
            break;
        }
    }
    if (!source.isValid())
        return;

    for (auto req : mPending) {
        if (req->source == source) {
            return;
        }
    }

    if (Server::instance()->options().options & Server::CompletionLogs)
        error() << "CODE COMPLETION reparse" << Rct::currentTimeString() << source.sourceFile();

    Request *request = new Request({ std::forward<Source>(source), Location(), Diagnose, -1, UnsavedFiles(), String(), std::shared_ptr<Connection>() });
    mPending.push_back(request);
    mCondition.notify_one();
}

String CompletionThread::Request::toString() const
{
    String ret = location.toString(Location::NoColor);
    auto it = unsavedFiles.find(source.sourceFile());
    const String *unsaved = (it != unsavedFiles.end()) ? &it->second : nullptr;
    if (unsaved) {
        ret += String::format<64>(" - Unsaved: %zu", unsaved->size());
    }

    struct {
        const char *name;
        const Flag flag;
    } const f[] = {
        { "Elisp", Elisp },
        { "XML", XML },
        { "JSON", JSON },
        { "IncludeMacros", IncludeMacros },
        { "WarmUp", WarmUp },
    };

    for (const auto &flag : f) {
        if (flags & flag.flag) {
            ret += String::format<64>(" - %s", flag.name);
        }
    }

    if (unsaved && !location.isNull()) {
        int line = location.line();
        int pos = 0;
        while (line > 1) {
            int p = unsaved->indexOf('\n', pos);
            if (p == -1) {
                pos = -1;
                break;
            }
            pos = p + 1;
            --line;
        }
        if (pos != -1) {
            int end = unsaved->indexOf('\n', pos);
            if (end == -1)
                end = unsaved->size();
            ret += String::format<1024>(" - Completing at %s:%d:%d line: [%s]",
                                        location.path().constData(),
                                        location.line(),
                                        location.column(),
                                        unsaved->mid(pos, end - pos).constData());
        }
    }

    return ret;
}

Source CompletionThread::findSource(const Set<uint32_t> &deps) const
{
    std::unique_lock<std::mutex> lock(mMutex);
    for (SourceFile *sourceFile = mCacheList.first(); sourceFile; sourceFile = sourceFile->next) {
        if (deps.contains(sourceFile->source.fileId)) {
            return sourceFile->source;
        }
    }
    return Source();
}

class TranslationUnitDiagnostics : public RTags::DiagnosticsProvider
{
public:
    TranslationUnitDiagnostics(uint32_t sourceFileId, CXTranslationUnit unit)
        : mSourceFileId(sourceFileId), mUnit(unit)
    {
        mIndexDataMessage.files()[sourceFileId] = IndexDataMessage::Visited;
    }
    virtual size_t unitCount() const override { return 1; }
    virtual size_t diagnosticCount(size_t) const override { return clang_getNumDiagnostics(mUnit); }
    virtual CXDiagnostic diagnostic(size_t, size_t idx) const override
    {
        return clang_getDiagnostic(mUnit, idx);
    }
    virtual Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = nullptr) override
    {
        if (blocked)
            *blocked = false;
        return Location(Location::insertFile(Path::resolved(file, Path::RealPath)), line, col);
    }
    virtual uint32_t sourceFileId() const override
    {
        return mSourceFileId;
    }
    virtual IndexDataMessage &indexDataMessage() override
    {
        return mIndexDataMessage;
    }
    virtual CXTranslationUnit unit(size_t) const override { return mUnit; }
private:
    uint32_t mSourceFileId;
    CXTranslationUnit mUnit;
    IndexDataMessage mIndexDataMessage;
};

class CompletionDiagnostics : public RTags::DiagnosticsProvider
{
public:
    CompletionDiagnostics(uint32_t sourceFileId, uint32_t completionFileId, CXCodeCompleteResults *results, CXTranslationUnit unit)
        : mSourceFileId(sourceFileId), mResults(results), mUnit(unit)
    {
        mIndexDataMessage.files()[completionFileId] = IndexDataMessage::Visited;
        mIndexDataMessage.files()[sourceFileId] = IndexDataMessage::Visited;
    }

    virtual size_t unitCount() const override
    {
        return 1;
    }

    virtual size_t diagnosticCount(size_t) const override
    {
        return clang_codeCompleteGetNumDiagnostics(mResults);
    }

    virtual CXDiagnostic diagnostic(size_t, size_t idx) const override
    {
        return clang_codeCompleteGetDiagnostic(mResults, idx);
    }

    virtual Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = nullptr) override
    {
        if (blocked)
            *blocked = false;
        return Location(Location::insertFile(file.resolved(Path::RealPath)), line, col);
    }

    virtual uint32_t sourceFileId() const override
    {
        return mSourceFileId;
    }

    virtual IndexDataMessage &indexDataMessage() override
    {
        return mIndexDataMessage;
    }

    virtual CXTranslationUnit unit(size_t) const override
    {
        return mUnit;
    }

    const uint32_t mSourceFileId;
    CXCodeCompleteResults *const mResults;
    CXTranslationUnit const mUnit;
    IndexDataMessage mIndexDataMessage;
};

class DiagnosticsEvent : public Event
{
public:
    DiagnosticsEvent(uint32_t sourceFileId, std::shared_ptr<Project> &&project, Diagnostics &&diagnostics)
        : mSourceFileId(sourceFileId), mProject(std::move(project)), mDiagnostics(std::move(diagnostics))
    {}

    virtual void exec() override
    {
        if (std::shared_ptr<Project> project = mProject.lock()) {
            project->updateDiagnostics(mSourceFileId, mDiagnostics);
        }
    }

    const uint32_t mSourceFileId;
    std::weak_ptr<Project> mProject;
    Diagnostics mDiagnostics;
};

void CompletionThread::processDiagnostics(const Request *request, CXCodeCompleteResults *results, CXTranslationUnit unit)
{
    assert(request);
    std::shared_ptr<Project> project = Server::instance()->currentProject();
    if (!project) {
        LOG() << "Processing diagnostics. No project";
        return;
    }
    const uint32_t sourceFileId = request->source.fileId;
    if (!project->hasSource(sourceFileId)) {
        LOG() << "Processing diagnostics. Project doesn't have" << Location::path(sourceFileId);
        return;
    }
    Diagnostics diagnostics;
    if (results) {
        LOG() << "processing diagnostics" << clang_codeCompleteGetNumDiagnostics(results)
              << clang_getNumDiagnostics(unit) << request->location << Location::path(request->source.fileId);
        CompletionDiagnostics diag(sourceFileId, request->location.fileId(), results, unit);
        diag.diagnose();
        // error() << "got diagnostics" << diag.indexDataMessage().diagnostics().size();
        diagnostics = std::move(diag.indexDataMessage().diagnostics());
    } else {
        TranslationUnitDiagnostics diag(sourceFileId, unit);
        LOG() << "processing diagnostics from translation unit" << diag.diagnosticCount(0)
              << Location::path(request->source.fileId);
        diag.diagnose();
        diagnostics = std::move(diag.indexDataMessage().diagnostics());
    }

    LOG() << "Sending diagnostics" << diagnostics.size();
    EventLoop::mainEventLoop()->post(new DiagnosticsEvent(sourceFileId, std::move(project), std::move(diagnostics)));
}
