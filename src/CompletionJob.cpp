#include "CompletionJob.h"
#include "QueryMessage.h"
#include "IndexerJob.h"
#include "Project.h"
#include <IndexerJobClang.h>
#include "Server.h"

CompletionJob::CompletionJob(const shared_ptr<Project> &project, Type type)
    : Job(WriteBuffered|WriteUnfiltered|QuietJob, project), mUnit(0),
      mLine(-1), mColumn(-1), mPos(-1), mParseCount(-1), mType(type)
{
}

void CompletionJob::init(CXTranslationUnit unit, const Path &path, const List<String> &args,
                         int line, int column, int pos, const String &unsaved, int parseCount)
{
    mUnit = unit;
    mPath = path;
    mArgs = args;
    mLine = line;
    mPos = pos;
    mParseCount = parseCount;
    mColumn = column;
    mUnsaved = unsaved;
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
        const int minLength = std::min(length, other.length);
        if (minLength) {
            const int cmp = strncmp(data, other.data, minLength);
            if (cmp < 0)
                return true;
            if (cmp > 0)
                return false;
        }

        return length > other.length;
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

void CompletionJob::processDiagnostics(CXCodeCompleteResults* results)
{
    if (!testLog(RTags::CompilationError))
        return;

    const unsigned int numDiags = clang_codeCompleteGetNumDiagnostics(results);
    for (unsigned int curDiag = 0; curDiag < numDiags; ++curDiag) {
        CXDiagnostic diagnostic = clang_codeCompleteGetDiagnostic(results, curDiag);
        const unsigned diagnosticOptions = (CXDiagnostic_DisplaySourceLocation|
                                            CXDiagnostic_DisplayColumn|
                                            CXDiagnostic_DisplaySourceRanges|
                                            CXDiagnostic_DisplayOption|
                                            CXDiagnostic_DisplayCategoryId|
                                            CXDiagnostic_DisplayCategoryName);
        const String text = RTags::eatString(clang_formatDiagnostic(diagnostic, diagnosticOptions));
        error() << "Got diagnostic" << curDiag << numDiags << text;
        log(RTags::CompilationError, "%s", text.constData());

        clang_disposeDiagnostic(diagnostic);
    }
    log(RTags::CompilationError, "$");
}

void CompletionJob::execute()
{
    StopWatch timer;
    CXUnsavedFile unsavedFile = { mUnsaved.isEmpty() ? 0 : mPath.constData(),
                                  mUnsaved.isEmpty() ? 0 : mUnsaved.constData(),
                                  static_cast<unsigned long>(mUnsaved.size()) };
    if (!mUnit) {
        String clangLine;
        RTags::parseTranslationUnit(mPath, mArgs, mUnit, Server::instance()->clangIndex(), clangLine,
                                    0, 0, &unsavedFile, 1);
        mParseCount = 1;
        if (!mUnit) {
            error() << "Failed to parse" << mPath << "Can't complete";
            return;
        }
    }
    // error() << "Completing" << mPath << mParseCount;
    assert(mParseCount >= 1 && mParseCount <= 2);
    if (mParseCount == 1) {
        RTags::reparseTranslationUnit(mUnit, &unsavedFile, 1);
        if (!mUnit) {
            mFinished(mPath, id());
            return;
        } else {
            ++mParseCount;
        }
    }

    CXCodeCompleteResults *results = clang_codeCompleteAt(mUnit, mPath.constData(), mLine, mColumn,
                                                          &unsavedFile, mUnsaved.isEmpty() ? 0 : 1,
                                                          CXCodeComplete_IncludeMacros
                                                          | CXCodeComplete_IncludeCodePatterns);
    if (results) {
        CompletionNode *nodes = new CompletionNode[results->NumResults];
        int nodeCount = 0;
        Map<Token, int> tokens;
        if (!mUnsaved.isEmpty()) {
            tokenize(mUnsaved.constData(), mUnsaved.size(), tokens);
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
            if (nodeCount > SendThreshold) {
                write("`");
            } else {
                qsort(nodes, nodeCount, sizeof(CompletionNode), compareCompletionNode);
                if (mType == Stream) {
                    write<128>("`%s %s", nodes[0].completion.constData(), nodes[0].signature.constData());
                } else {
                    write<128>("%s %s", nodes[0].completion.constData(), nodes[0].signature.constData());
                }
                for (int i=1; i<nodeCount; ++i) {
                    write<128>("%s %s", nodes[i].completion.constData(), nodes[i].signature.constData());
                }
            }
        }

        warning() << "Wrote" << ((nodeCount > SendThreshold) ? -1 : nodeCount) << "completions for"
                  << String::format<128>("%s:%d:%d", mPath.constData(), mLine, mColumn)
                  << "in" << timer.elapsed() << "ms" << mArgs;
        // const unsigned diagnosticCount = clang_getNumDiagnostics(mUnit);
        // for (unsigned i=0; i<diagnosticCount; ++i) {
        //     CXDiagnostic diagnostic = clang_getDiagnostic(mUnit, i);
        //     const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);
        //     const String msg = RTags::eatString(clang_getDiagnosticSpelling(diagnostic));
        //     CXFile file;
        //     unsigned line, col;
        //     clang_getSpellingLocation(clang_getDiagnosticLocation(diagnostic), &file, &line, &col, 0);

        //     error() << i << diagnosticCount << severity << msg
        //             << String::format<128>("%s:%d:%d", RTags::eatString(clang_getFileName(file)).constData(),
        //                                    line, col);
        //     clang_disposeDiagnostic(diagnostic);
        // }

        delete[] nodes;

        //processDiagnostics(results);

        clang_disposeCodeCompleteResults(results);
        shared_ptr<Project> proj = project();
        if (proj) {
            // error() << "Adding to cache" << mParseCount << mPath;
            proj->addToCache(mPath, mArgs, mUnit, mParseCount);
        }
    }
    mFinished(mPath, id());
}
