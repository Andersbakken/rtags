#ifndef IndexerJob_h
#define IndexerJob_h

#include "Indexer.h"
#include "RTags.h"
#include "Job.h"
#include "Str.h"
#include "ThreadPool.h"
#include "Mutex.h"
#include <clang-c/Index.h>

struct IndexData {
    ReferenceMap references;
    SymbolMap symbols;
    SymbolNameMap symbolNames;
    DependencyMap dependencies;
    ByteArray message;
    UsrMap usrMap;
    FixItMap fixIts;
    DiagnosticsMap diagnostics;
};

class IndexerJob : public Job
{
public:
    enum Flag {
        Makefile = 0x01,
        Dirty = 0x02,
        Priorities = Dirty|Makefile,
        IgnorePrintfFixits = 0x10
    };
    IndexerJob(const shared_ptr<Indexer> &indexer, unsigned flags,
               const Path &input, const List<ByteArray> &args,
               CXIndex index = 0 , CXTranslationUnit unit = 0);
    IndexerJob(const QueryMessage &msg, const shared_ptr<Project> &project,
               const Path &input, const List<ByteArray> &arguments);

    int priority() const { return mFlags & Priorities; }
    shared_ptr<IndexData> data() const { return mData; }
    CXTranslationUnit takeTranslationUnit();
    CXIndex takeIndex();
    uint32_t fileId() const { return mFileId; }
    Path path() const { return mPath; }
    bool isAborted() { return !indexer() && !project(); }
    void abort() { MutexLocker lock(&mMutex); mIndexer.reset(); resetProject(); }
    bool abortIfStarted();
    List<ByteArray> arguments() const { return mArgs; }
    shared_ptr<Indexer> indexer() { MutexLocker lock(&mMutex); return mIndexer.lock(); }
    time_t parseTime() const { return mParseTime; }
private:
    void parse();
    void visit();
    void diagnose();

    virtual void execute();

    Location createLocation(const CXSourceLocation &location, bool *blocked);
    inline Location createLocation(const CXCursor &cursor, bool *blocked)
    {
        return createLocation(clang_getCursorLocation(cursor), blocked);
    }
    static Location createLocation(const CXCursor &cursor);
    ByteArray addNamePermutations(const CXCursor &cursor, const Location &location);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static CXChildVisitResult verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData);
    static CXChildVisitResult dumpVisitor(CXCursor cursor, CXCursor, CXClientData userData);

    static void inclusionVisitor(CXFile included_file, CXSourceLocation *include_stack,
                                 unsigned include_len, CXClientData client_data);

    bool handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location);
    void handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &loc,
                         const CXCursor &reference, const CXCursor &parent);
    void handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location);
    Location findByUSR(const CXCursor &cursor, CXCursorKind kind, const Location &loc) const;
    void addOverriddenCursors(const CXCursor& cursor, const Location& location, List<CursorInfo*>& infos);

    unsigned mFlags;
    time_t mTimeStamp;

    enum PathState {
        Unset,
        Index,
        DontIndex
    };
    Map<uint32_t, PathState> mPaths;

    const Path mPath;
    const uint32_t mFileId;
    const List<ByteArray> mArgs;

    Mutex mMutex;
    weak_ptr<Indexer> mIndexer;

    CXTranslationUnit mUnit;
    CXIndex mIndex;

    Map<ByteArray, uint32_t> mFileIds;

    ByteArray mClangLine;

    Timer mTimer;
    shared_ptr<IndexData> mData;

    bool mDump;

    time_t mParseTime;

    bool mStarted;
};

#endif
