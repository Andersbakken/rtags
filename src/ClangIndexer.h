#ifndef ClangIndexer_h
#define ClangIndexer_h

#include <rct/StopWatch.h>
#include <rct/Hash.h>
#include <rct/Serializer.h>
#include <rct/Path.h>
#include <rct/Connection.h>
#include <sys/stat.h>
#include "IndexerJob.h"
#include "RTagsClang.h"

struct Unit;
class IndexData;
class ClangIndexer
{
public:
    ClangIndexer();
    ~ClangIndexer();

    bool exec(const String &data);
private:
    bool diagnose();
    bool visit();
    bool parse();
    bool loadFromCache();
    String shaFile(const Path &path) const;

    void addFileSymbol(uint32_t file);
    inline Location createLocation(const CXSourceLocation &location, bool *blocked = 0)
    {
        CXString fileName;
        unsigned line, col;
        CXFile file;
        clang_getSpellingLocation(location, &file, &line, &col, 0);
        if (file) {
            fileName = clang_getFileName(file);
        } else {
            if (blocked)
                *blocked = false;
            return Location();
        }
        const char *fn = clang_getCString(fileName);
        assert(fn);
        if (!*fn || !strcmp("<built-in>", fn) || !strcmp("<command line>", fn)) {
            if (blocked)
                *blocked = false;
            clang_disposeString(fileName);
            return Location();
        }
        if (!strcmp(fn, mLastFile.constData())) {
            clang_disposeString(fileName);
            if (mLastBlocked && blocked) {
                *blocked = true;
                return Location();
            } else if (blocked) {
                *blocked = false;
            }

            return Location(mLastFileId, line, col);
        }
        const Path path = RTags::eatString(fileName);
        const Location ret = createLocation(path, line, col, blocked);
        if (blocked) {
            mLastBlocked = *blocked;
            mLastFileId = ret.fileId();
            mLastFile = path;
        }
        return ret;
    }
    Location createLocation(CXFile file, unsigned line, unsigned col, bool *blocked = 0)
    {
        if (blocked)
            *blocked = false;
        if (!file)
            return Location();

        CXString fn = clang_getFileName(file);
        const char *cstr = clang_getCString(fn);
        if (!cstr) {
            clang_disposeString(fn);
            return Location();
        }
        const Path p = Path::resolved(cstr);
        clang_disposeString(fn);
        return createLocation(p, line, col, blocked);
    }
    inline Location createLocation(const CXCursor &cursor, bool *blocked = 0)
    {
        const CXSourceRange range = clang_Cursor_getSpellingNameRange(cursor, 0, 0);
        if (clang_Range_isNull(range))
            return Location();
        return createLocation(clang_getRangeStart(range), blocked);
    }
    Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = 0);
    String addNamePermutations(const CXCursor &cursor, const Location &location);

    CXCursor resolveAutoTypeRef(const CXCursor &cursor) const;

    bool handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location);
    std::shared_ptr<CursorInfo> handleReference(const CXCursor &cursor, CXCursorKind kind,
                                                const Location &loc, const CXCursor &reference,
                                                const CXCursor &parent);
    void handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location);
    Location findByUSR(const CXCursor &cursor, CXCursorKind kind, const Location &loc) const;
    void addOverriddenCursors(const CXCursor& cursor, const Location& location, List<CursorInfo*>& infos);
    void superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                  const Location &location, const CXCursor &ref,
                                                  const CXCursor &parent);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static CXChildVisitResult verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData);

    static void inclusionVisitor(CXFile includedFile, CXSourceLocation *includeStack,
                                 unsigned includeLen, CXClientData userData);

    void onMessage(const std::shared_ptr<Message> &msg, Connection *conn);

    Path mProject;
    Source mSource;
    Path mSourceFile;
    std::shared_ptr<IndexData> mData;
    CXTranslationUnit mClangUnit;
    bool mLoadedFromCache;
    CXIndex mIndex;
    CXCursor mLastCursor;
    String mClangLine;
    uint32_t mVisitFileResponseMessageFileId;
    bool mVisitFileResponseMessageVisit;
    Path mSocketFile, mASTCacheDir;
    StopWatch mTimer;
    int mParseDuration, mVisitDuration, mBlocked, mAllowed,
        mIndexed, mVisitFileTimeout, mIndexerMessageTimeout, mFileIdsQueried;
    UnsavedFiles mUnsavedFiles;
    Connection mConnection;
    FILE *mLogFile;
    uint32_t mLastFileId;
    bool mLastBlocked;
    Path mLastFile;
};

#endif
