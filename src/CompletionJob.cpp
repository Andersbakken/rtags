#include "CompletionJob.h"
#include "IndexerJob.h"
#include "EventLoop.h"

CompletionJob::CompletionJob(const shared_ptr<Project> &project)
    : Job(WriteBuffered|WriteUnfiltered, project), mIndex(0), mUnit(0), mLine(-1), mColumn(-1), mPos(-1)
{
}

void CompletionJob::init(CXIndex index, CXTranslationUnit unit, const Path &path, const List<ByteArray> &args,
                         int line, int column, int pos, const ByteArray &unsaved)
{
    mIndex = index;
    mUnit = unit;
    mPath = path;
    mArgs = args;
    mLine = line;
    mPos = pos;
    mColumn = column;
    mUnsaved = unsaved;
}

// static inline const char *completionChunkKindToString(CXCompletionChunkKind kind)
// {
//     switch (kind) {
//     case CXCompletionChunk_Optional: return "Optional";
//     case CXCompletionChunk_TypedText: return "TypedText";
//     case CXCompletionChunk_Text: return "Text";
//     case CXCompletionChunk_Placeholder: return "Placeholder";
//     case CXCompletionChunk_Informative: return "Informative";
//     case CXCompletionChunk_CurrentParameter: return "CurrentParameter";
//     case CXCompletionChunk_LeftParen: return "LeftParen";
//     case CXCompletionChunk_RightParen: return "RightParen";
//     case CXCompletionChunk_LeftBracket: return "LeftBracket";
//     case CXCompletionChunk_RightBracket: return "RightBracket";
//     case CXCompletionChunk_LeftBrace: return "LeftBrace";
//     case CXCompletionChunk_RightBrace: return "RightBrace";
//     case CXCompletionChunk_LeftAngle: return "LeftAngle";
//     case CXCompletionChunk_RightAngle: return "RightAngle";
//     case CXCompletionChunk_Comma: return "Comma";
//     case CXCompletionChunk_ResultType: return "ResultType";
//     case CXCompletionChunk_Colon: return "Colon";
//     case CXCompletionChunk_SemiColon: return "SemiColon";
//     case CXCompletionChunk_Equal: return "Equal";
//     case CXCompletionChunk_HorizontalSpace: return "HorizontalSpace";
//     case CXCompletionChunk_VerticalSpace: return "VerticalSpace";
//     };
//     return "";
// }

static int compareCompletionResult(const void *left, const void *right)
{
    const int l = clang_getCompletionPriority(reinterpret_cast<const CXCompletionResult*>(left)->CompletionString);
    const int r = clang_getCompletionPriority(reinterpret_cast<const CXCompletionResult*>(right)->CompletionString);
    if (l != r)
        return l < r ? -1 : 1;
    return 0;
}

static inline bool isPartOfSymbol(char ch)
{
    return isalnum(ch) || ch == '_';
}

static inline CXCursor findContainer(CXCursor cursor)
{
    do {
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_FunctionDecl:
        case CXCursor_FunctionTemplate:
        case CXCursor_CXXMethod:
        case CXCursor_Constructor:
        case CXCursor_Destructor:
        case CXCursor_ClassDecl:
        case CXCursor_ClassTemplate:
        case CXCursor_StructDecl:
        case CXCursor_Namespace:
            return cursor;
        default:
            break;
        }
        cursor = clang_getCursorSemanticParent(cursor);
    } while (!clang_isInvalid(clang_getCursorKind(cursor)));

    assert(clang_equalCursors(cursor, clang_getNullCursor()));

    return cursor;
}

static inline ByteArray fullyQualifiedName(CXCursor cursor)
{
    ByteArray ret;
    ret.reserve(128);
    CXCursorKind kind = clang_getCursorKind(cursor);

    do {
        if (!ret.isEmpty())
            ret.prepend("::");
        ret.prepend(RTags::eatString(clang_getCursorDisplayName(cursor)));
        if (!RTags::needsQualifiers(kind))
            break;
        cursor = clang_getCursorSemanticParent(cursor);
        kind = clang_getCursorKind(cursor);
    } while (RTags::isContainer(kind));

    return ret;
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

    ByteArray current, parent;
    if (CXFile f = clang_getFile(mUnit, mPath.constData())) {
        // printf("%p %s\n", f, f ? RTags::eatString(clang_getFileName(f)).constData() : "none");

        CXSourceLocation loc = clang_getLocationForOffset(mUnit, f, mPos);
        CXCursor cursor = clang_getCursor(mUnit, loc);
        // error() << cursor;
        if (!clang_isInvalid(clang_getCursorKind(cursor))) {
            CXCursor ref = clang_getCursorReferenced(cursor);
            if (!clang_isInvalid(clang_getCursorKind(ref))) {
                current = fullyQualifiedName(ref);
            } else {
                current = fullyQualifiedName(cursor);
            }
            parent = fullyQualifiedName(findContainer(cursor));
        } else if (!mUnsaved.isEmpty()) {
            CXCursor parents[2];
            const int offsets[] = { -1, 1 };
            bool valid[2] = { false, false };
            for (int i=0; i<2; ++i) {
                int pos = mPos;
                while (pos >= 0 && pos < mUnsaved.size() && isspace(mUnsaved.at(pos)))
                    pos += offsets[i];
                if (pos >= 0 && pos < mUnsaved.size()) {
                    loc = clang_getLocationForOffset(mUnit, f, mPos);
                    parents[i] = findContainer(clang_getCursor(mUnit, loc));
                    valid[i] = !clang_isInvalid(clang_getCursorKind(parents[i]));
                }
            }
            if ((valid[0] || valid[1]) && (clang_equalCursors(parents[0], parents[1]) || valid[0] != valid[1])) {
                parent = fullyQualifiedName(valid[0] ? parents[0] : parents[1]);
            }
        }
    }

    error("[%s][%s]", current.constData(), parent.constData());

    if (results) {
        qsort(results->Results, results->NumResults, sizeof(CXCompletionResult), compareCompletionResult);
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

            ByteArray signature, completion;
            signature.reserve(256);
            const int chunkCount = clang_getNumCompletionChunks(string);
            bool ok = true;
            for (int j=0; j<chunkCount; ++j) {
                const CXCompletionChunkKind chunkKind = clang_getCompletionChunkKind(string, j);
                if (chunkKind == CXCompletionChunk_TypedText) {
                    completion = RTags::eatString(clang_getCompletionChunkText(string, j));
                    if (completion.size() > 8 && completion.startsWith("operator") && !isPartOfSymbol(completion.at(8))) {
                        ok = false;
                        break;
                    }
                    signature.append(completion);
                } else {
                    signature.append(RTags::eatString(clang_getCompletionChunkText(string, j)));
                    if (chunkKind == CXCompletionChunk_ResultType)
                        signature.append(' ');
                }
            }

            int pos = completion.size() - 1;
            while (pos >= 0 && isspace(completion.at(pos)))
                --pos;
            if (ok && pos >= 0) {
                completion.truncate(pos + 1);
                assert(!completion.contains(' '));
                write<128>("%s %s", completion.constData(), signature.constData());
            }
        }


        clang_disposeCodeCompleteResults(results);
        project()->indexer->addToCache(mPath, mArgs, mIndex, mUnit);
    }
    mFinished(mPath);
}
