#include "CompletionJob.h"
#include "IndexerJob.h"
#include "EventLoop.h"

CompletionJob::CompletionJob(const shared_ptr<Project> &project)
    : Job(0, project), mIndex(0), mUnit(0), mLine(-1), mColumn(-1)
{}

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

void CompletionJob::execute()
{
    // char *content;
    // int len = mPath.readAll(content);
    // mUnsaved = content;
    // delete[] content;

    // error() << mUnsaved.size();

    // mUnsaved.truncate(mPath.readAll(mUnsaved.data(), mUnsaved.size()));
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
        clang_sortCodeCompletionResults(results->Results, results->NumResults);
        error() << "Got some results for" << mPath << mLine << mColumn << results->NumResults;
        for (unsigned i=0; i<results->NumResults; ++i) {
            CXCompletionString &string = results->Results[i].CompletionString;
            const int chunkCount = clang_getNumCompletionChunks(string);
            const int priority = clang_getCompletionPriority(string);
            const CXAvailabilityKind availabilityKind = clang_getCompletionAvailability(string);
            error() << (i + 1) << "of" << results->NumResults << results->Results[i].CursorKind << "priority" << priority << "availabilityKind" << availabilityKind;

            for (int j=0; j<chunkCount; ++j) {
                const CXCompletionChunkKind chunkKind = clang_getCompletionChunkKind(string, j);
                CXString chunkText = clang_getCompletionChunkText(string, j);
                error() << "    chunk " << (j + 1) << "of" << chunkCount << completionChunkKindToString(chunkKind)
                        << ByteArray::snprintf<64>("[%s]", RTags::eatString(chunkText).constData());
            }

            const int annotationCount = clang_getCompletionNumAnnotations(string);
            for (int j=0; j<annotationCount; ++j) {
                error() << "    annotation " << (j + 1) << "of" << annotationCount
                        << ByteArray::snprintf<64>("[%s]", RTags::eatString(clang_getCompletionAnnotation(string, j)).constData());
            }

            error() << "-------------------";
        }

        clang_disposeCodeCompleteResults(results);
        project()->indexer->addToCache(mPath, mArgs, mIndex, mUnit);
    }
}
