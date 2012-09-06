#ifndef IndexerJob_h
#define IndexerJob_h

#include "Indexer.h"
#include "RTags.h"
#include "Job.h"
#include "Str.h"
#include "ThreadPool.h"
#include "AbortInterface.h"
#include <clang-c/Index.h>

struct IndexData {
    ReferenceMap references;
    SymbolMap symbols;
    SymbolNameMap symbolNames;
    DependencyMap dependencies;
    FileInformation fileInformation;
    FixitMap fixIts;
    DiagnosticsMap diagnostics;
    ByteArray message;
};

class IndexerJob : public ThreadPool::Job
{
public:
    enum Flag {
        Makefile = 0x1,
        Dirty = 0x02,
        Priorities = Dirty|Makefile
    };
    IndexerJob(Indexer *indexer, unsigned flags,
               const Path &input, const List<ByteArray> &arguments);
    int priority() const { return mFlags & Priorities; }
    shared_ptr<IndexData> data() const { return mData; }
    signalslot::Signal1<IndexerJob*> &finished() { return mFinished; }
    bool restart(time_t time, const Set<uint32_t> &dirtyFiles, const Map<Path, List<ByteArray> > &pendingFiles);
    uint32_t fileId() const { return mFileId; }
    void abort(Set<uint32_t> *visitedFiles = 0);
    Path path() const { return mPath; }
    inline bool isAborted() const
    {
        MutexLocker lock(&mMutex);
        return mAborted;
    }
private:
    void parse();
    void visit();
    void diagnose();

    virtual void run();

    enum CreateLocationState {
        Failed,
        Allowed,
        Blocked,
        Aborted
    };
    Location createLocation(const CXCursor &cursor, CreateLocationState *blocked);
    static Location createLocation(const CXCursor &cursor);
    ByteArray addNamePermutations(const CXCursor &cursor, const Location &location, bool addToDb);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static CXChildVisitResult verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData);

    static void inclusionVisitor(CXFile included_file, CXSourceLocation *include_stack,
                                 unsigned include_len, CXClientData client_data);

    void handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location, const Location *refLoc = 0);
    void handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &loc);
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
    Indexer *mIndexer;

    CXTranslationUnit mUnit;
    CXIndex mIndex;

    Map<ByteArray, uint32_t> mFileIds;

    signalslot::Signal1<IndexerJob*> mFinished;

    mutable Mutex mMutex;
    bool mAborted;

    ByteArray mClangLine;

    Timer mTimer;
    shared_ptr<IndexData> mData;
};

#endif
