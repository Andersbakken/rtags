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

struct Cpp;
class IndexData;
class ClangIndexer
{
public:
    ClangIndexer();
    ~ClangIndexer();

    bool connect(const Path &serverFile, int timeout);
    bool connect(const String &hostName, uint16_t port, int timeout);

    bool index(uint32_t flags, const Source &source,
               const std::shared_ptr<Cpp> &cpp, const Path &project);
    int visitFileTimeout() const { return mVisitFileTimeout; }
    void setVisitFileTimeout(int visitFileTimeout) { mVisitFileTimeout = visitFileTimeout; }
    int indexerMessageTimeout() const { return mIndexerMessageTimeout; }
    void setIndexerMessageTimeout(int indexerMessageTimeout) { mIndexerMessageTimeout = indexerMessageTimeout; }
    void setJobId(uint64_t id) { mId = id; }
private:
    bool diagnose();
    bool visit();
    bool parse();

    void addFileSymbol(uint32_t file);
    inline Location createLocation(const CXSourceLocation &location, bool *blocked)
    {
        CXString file;
        unsigned line, col;
        clang_getPresumedLocation(location, &file, &line, &col);
        const char *fn = clang_getCString(file);
        assert(fn);
        if (!*fn || !strcmp("<built-in>", fn) || !strcmp("<command line>", fn)) {
            if (blocked)
                *blocked = false;
            clang_disposeString(file);
            return Location();
        }
        if (!strcmp(fn, mLastFile.constData())) {
            clang_disposeString(file);
            if (mLastBlocked && blocked) {
                *blocked = true;
                return Location();
            } else if (blocked) {
                *blocked = false;
            }

            return Location(mLastFileId, line, col);
        }
        const Path path = RTags::eatString(file);
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
        const Path p = mLocalJob ? Path::resolved(cstr) : Path(cstr);
        clang_disposeString(fn);
        return createLocation(p, line, col, blocked);
    }
    inline Location createLocation(const CXCursor &cursor, bool *blocked = 0)
    {
        return createLocation(clang_getCursorLocation(cursor), blocked);
    }
    Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = 0);
    String addNamePermutations(const CXCursor &cursor, const Location &location);

    bool handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location);
    void handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &loc,
                         const CXCursor &reference, const CXCursor &parent);
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

    void onMessage(Message *msg, Connection *conn);

    Path mProject;
    std::shared_ptr<IndexData> mData;
    Source mSource;
    CXTranslationUnit mUnit;
    CXIndex mIndex;
    CXCursor mLastCursor;
    String mClangLine;
    uint32_t mVisitFileResponseMessageFileId;
    bool mVisitFileResponseMessageVisit;
    Path mSocketFile;
    StopWatch mTimer;
    int mParseDuration, mVisitDuration, mBlocked, mAllowed,
        mIndexed, mVisitFileTimeout, mIndexerMessageTimeout, mFileIdsQueried;
    Path mVisitFileResponseMessageResolved;
    std::shared_ptr<Cpp> mCpp;
    String mPreprocessed;
    Connection mConnection;
    uint64_t mId;
    FILE *mLogFile;
    bool mLocalJob;
    uint32_t mLastFileId;
    bool mLastBlocked;
    Path mLastFile;
};

#endif
