#include "CompletionJob.h"
#include "IndexerJob.h"
#include "EventLoop.h"

CompletionJob::CompletionJob(const shared_ptr<Project> &project)
    : Job(WriteBuffered|WriteUnfiltered, project), mIndex(0), mUnit(0), mLine(-1), mColumn(-1)
{
}

void CompletionJob::init(CXIndex index, CXTranslationUnit unit, const Path &path, const List<ByteArray> &args,
                         int line, int column, const ByteArray &unsaved)
{
    mIndex = index;
    mUnit = unit;
    mPath = path;
    mArgs = args;
    mLine = line;
    mColumn = column;
    mUnsaved = unsaved;
}

static inline const char *completionChunkKindToString(CXCompletionChunkKind kind)
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
    };
    return "";
}

static int compareCompletionResult(const void *left, const void *right)
{
    const int l = clang_getCompletionPriority(reinterpret_cast<const CXCompletionResult*>(left)->CompletionString);
    const int r = clang_getCompletionPriority(reinterpret_cast<const CXCompletionResult*>(right)->CompletionString);
    if (l != r)
        return l < r ? -1 : 1;
    return 0;
}

void CompletionJob::execute()
{
    CXUnsavedFile unsavedFile = { mUnsaved.isEmpty() ? 0 : mPath.constData(),
                                  mUnsaved.isEmpty() ? 0 : mUnsaved.constData(),
                                  static_cast<unsigned long>(mUnsaved.size()) };

    CXCodeCompleteResults *results = clang_codeCompleteAt(mUnit,
                                                          mPath.constData(),
                                                          mLine, mColumn,
                                                          &unsavedFile,
                                                          mUnsaved.isEmpty() ? 0 : 1,
                                                          clang_defaultCodeCompleteOptions());

    if (results) {
        qsort(results->Results, results->NumResults, sizeof(CXCompletionResult), compareCompletionResult);
        bool sentHeader = false;
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

            const int chunkCount = clang_getNumCompletionChunks(string);
            for (int j=0; j<chunkCount; ++j) {
                const CXCompletionChunkKind chunkKind = clang_getCompletionChunkKind(string, j);
                if (chunkKind == CXCompletionChunk_TypedText) {
                    const ByteArray chunkText = RTags::eatString(clang_getCompletionChunkText(string, j));
                    if (!sentHeader) {
                        write<64>("`%s", chunkText.constData());
                        sentHeader = true;
                    } else {
                        write(chunkText);
                    }
                    break;
                }
            }
        }

        clang_disposeCodeCompleteResults(results);
        project()->indexer->addToCache(mPath, mArgs, mIndex, mUnit);
    }
    mFinished(mPath);
}
