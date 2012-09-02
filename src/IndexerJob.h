#ifndef IndexerJob_h
#define IndexerJob_h

#include "Indexer.h"
#include "RTags.h"
#include "Job.h"
#include "Str.h"
#include "ThreadPool.h"
#include "AbortInterface.h"
#include <clang-c/Index.h>

class IndexerJob : public ThreadPool::Job
{
public:
    enum Flag {
        Makefile = 0x1,
        Dirty = 0x02,
        Priorities = Dirty|Makefile
    };
    IndexerJob(Indexer *indexer, unsigned flags,
               const Path &input, const List<ByteArray> &arguments,
               const Set<uint32_t> &dirtyFiles, const Map<Path, List<ByteArray> > &pendingFiles);
    int priority() const { return mFlags & Priorities; }
    signalslot::Signal1<IndexerJob*> &finished() { return mFinished; }
    bool restart(time_t time, const Set<uint32_t> &dirtyFiles, const Map<Path, List<ByteArray> > &pendingFiles);
    uint32_t fileId() const { return mFileId; }
    unsigned abort();
    bool isAborted() const { MutexLocker lock(&mStateMutex); return mState & Aborted; }
    Set<uint32_t> visitedFiles() const; // not caching
    const ByteArray &message() const { return mMessage; }
    Path path() const { return mIn; }

    enum State {
        Starting = 0x001,
        Parsing = 0x002,
        Failed = 0x004,
        Visiting = 0x008,
        Dirtying = 0x010,
        Writing = 0x020,
        Finishing = 0x040,
        Done = 0x080,
        Aborted = 0x100,
        Reparse = 0x200
    };
    
private:
    void diagnose();

    unsigned parse();
    unsigned visit();
    unsigned dirty(const Set<uint32_t> &dirty);
    unsigned write();
    unsigned finish(const Map<Path, List<ByteArray> > &pendingSources);

    virtual void run();

    Location createLocation(const CXCursor &cursor, bool *blocked);
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
    unsigned setState(unsigned state, Set<uint32_t> *dirty = 0, Map<Path, List<ByteArray> > *pendingSources = 0);

    unsigned mFlags;
    time_t mTimeStamp;

    SymbolMap mSymbols;
    SymbolNameMap mSymbolNames;

    enum PathState {
        Unset,
        Index,
        DontIndex
    };
    Map<uint32_t, PathState> mPaths;
    Map<Location, std::pair<int, ByteArray> > mFixIts;
    Map<uint32_t, List<ByteArray> > mDiagnostics;

    Map<Str, Location> mHeaderMap;
    ReferenceMap mReferences;
    const Path mIn;
    const uint32_t mFileId;
    const List<ByteArray> mArgs;
    DependencyMap mDependencies;
    Indexer *mIndexer;

    CXTranslationUnit mUnit;
    CXIndex mIndex;

    ByteArray mMessage;

    Map<ByteArray, uint32_t> mFileIds;

    signalslot::Signal1<IndexerJob*> mFinished;

    unsigned mState;
    mutable Mutex mStateMutex;

    Set<uint32_t> mDirty;
    Map<Path, List<ByteArray> > mPendingSources;

    ByteArray mClangLine;

    Timer mTimer;

    int mVisitCount;
};

#endif
