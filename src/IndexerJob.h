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
    FixitMap fixIts;
    DiagnosticsMap diagnostics;
    ByteArray message;
};

class IndexerJob : public Job
{
public:
    enum Flag {
        Makefile = 0x1,
        Dirty = 0x02,
        Priorities = Dirty|Makefile
    };
    IndexerJob(const shared_ptr<Indexer> &indexer, unsigned flags,
               const Path &input, const List<ByteArray> &arguments);
    IndexerJob(const QueryMessage &msg, const shared_ptr<Project> &project,
               const Path &input, const List<ByteArray> &arguments);

    int priority() const { return mFlags & Priorities; }
    shared_ptr<IndexData> data() const { return mData; }
    uint32_t fileId() const { return mFileId; }
    Path path() const { return mPath; }
    bool isAborted() { return !indexer() && !project(); }
    void abort() { MutexLocker lock(&mMutex); mIndexer.reset(); resetProject(); }
    bool abortIfStarted();
    shared_ptr<Indexer> indexer() { MutexLocker lock(&mMutex); return mIndexer.lock(); }
    time_t parseTime() const { return mParseTime; }
private:
    void parse();
    void visit();
    void diagnose();

    virtual void execute();

    Location createLocation(const CXCursor &cursor, bool *blocked);
    static Location createLocation(const CXCursor &cursor);
    ByteArray addNamePermutations(const CXCursor &cursor, const Location &location);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static CXChildVisitResult verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData);
    static CXChildVisitResult dumpVisitor(CXCursor cursor, CXCursor, CXClientData userData);

    static void inclusionVisitor(CXFile included_file, CXSourceLocation *include_stack,
                                 unsigned include_len, CXClientData client_data);

    void handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location);
    void handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &loc, const CXCursor &reference);
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

    Map<Str, Location> mHeaderMap;
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
