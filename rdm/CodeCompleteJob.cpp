#include "CodeCompleteJob.h"
#include "UnitCache.h"
#include <Rdm.h>

CodeCompleteJob::CodeCompleteJob(int i, const RTags::Location &loc,
                                 const QHash<Path, QByteArray> &unsaved)
    : id(i), location(loc), unsavedFiles(unsaved)
{
}

CodeCompleteJob::~CodeCompleteJob()
{
}

static inline bool isOperator(const char *str, int len)
{
    if (len >= 9 && !strncmp(str, "operator", 8)) {
        const char ch = str[8];
        if (!isalnum(ch) && ch != '_')
            return true;
    }
    return false;
}

static inline int length(int num)
{
    int count = 1;
    while (num > 10) {
        ++count;
        num /= 10;
    }
    return count;
}

void CodeCompleteJob::run()
{
    CachedUnit locker(location.path, UnitCache::AST | UnitCache::Memory | UnitCache::Source);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        Rdm::FirstUnitData first;
        first.fileName = location.path;
        Rdm::visitIncluderFiles(location.path, Rdm::visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            warning("codecomplete: no unit for %s", location.path.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXUnsavedFile *unsaved = 0;
    if (unsavedFiles.size()) {
        unsaved = new CXUnsavedFile[unsavedFiles.size()];
        int i = 0;
        for (QHash<Path, QByteArray>::const_iterator it = unsavedFiles.begin(); it != unsavedFiles.end(); ++it) {
            unsaved[i].Filename = it.key().constData();
            unsaved[i].Contents = it.value().constData();
            unsaved[i].Length = it.value().size();
        }
    }
    int line, col;
    if (location.offset == -1) {
        line = location.line;
        col = location.column;
    } else {
        Q_ASSERT(0 && "clang_codeCompleteAt doesn't support offset. I could open the file and find the line/col but really... Do it in elisp.");
        // make gcc not warn
        line = 0;
        col = 0;
    }
    CXCodeCompleteResults *results = clang_codeCompleteAt(data->unit,
                                                          data->fileName.constData(),
                                                          line, col,
                                                          unsaved, unsavedFiles.size(),
                                                          clang_defaultCodeCompleteOptions());
    if (unsaved)
        delete[] unsaved;

    log(1) << results << results->NumResults << unsavedFiles.keys();
    QList<QByteArray> ret;
    for (unsigned int i = 0; i < results->NumResults; ++i) {
        const CXCompletionString& str = results->Results[i].CompletionString;
        if (clang_getCompletionAvailability(str) != CXAvailability_Available)
            continue;
        int priority;
        // log(1) << "stuff" << i << clang_getCompletionPriority(str);
        // for (int b=0; b<clang_getCompletionNumAnnotations(str); ++b) {
        //     log(1) << b << RTags::Rdm::eatString(clang_getCompletionAnnotation(str, b));
        // }

        switch (results->Results[i].CursorKind) {
        case CXCursor_Destructor:
        case CXCursor_ClassDecl:
            priority = 65;
            break;
        default:
            priority = clang_getCompletionPriority(str);
            break;
        }
        // printf("Got thing %s %d\n" , RTags::Rdm::eatString(clang_getCursorKindSpelling(results->Results[i].CursorKind)).constData(),
        //        clang_getNumCompletionChunks(str));

        for (unsigned int j = 0; j < clang_getNumCompletionChunks(str); ++j) {
            if (clang_getCompletionChunkKind(str, j) != CXCompletionChunk_TypedText)
                continue;

            CXString out = clang_getCompletionChunkText(str, j);
            const char *cstr = clang_getCString(out);
            const int len = strlen(cstr);
            if (isOperator(cstr, len)) {
                priority = 65;
            }
            QByteArray b(len + length(priority) + 2, 0);
            snprintf(b.data(), b.size(), "%s %d", cstr, priority);
            ret.append(b);
            clang_disposeString(out);
        }
    }

    clang_disposeCodeCompleteResults(results);
    emit complete(id, ret);
}

