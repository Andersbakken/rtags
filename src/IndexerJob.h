#ifndef IndexerJob_h
#define IndexerJob_h

#include "Indexer.h"
#include "RTags.h"
#include "Job.h"
#include "Str.h"
#include "ThreadPool.h"
#include "AbortInterface.h"
#include <clang-c/Index.h>

class IndexerJob : public ThreadPool::Job, public AbortInterface
{
public:
    enum Flag {
        DirtyPch = 0x04,
        Dirty = 0x02,
        Makefile = 0x1,
        Priorities = DirtyPch|Dirty|Makefile
    };
    IndexerJob(Indexer *indexer, unsigned flags,
               const Path &input, const List<ByteArray> &arguments);
    int priority() const { return mFlags & Priorities; }
    virtual void run();
    void execute();

    Location createLocation(const CXCursor &cursor, bool *blocked);
    static Location createLocation(const CXCursor &cursor);
    ByteArray addNamePermutations(const CXCursor &cursor, const Location &location, bool addToDb);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static void inclusionVisitor(CXFile included_file, CXSourceLocation *include_stack,
                                 unsigned include_len, CXClientData client_data);

    signalslot::Signal1<IndexerJob*> &finished() { return mFinished; }

    struct Cursor {
        CXCursor cursor;
        Location location;
        CXCursorKind kind;
    };

    void handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location, const Location *refLoc = 0);
    void handleReference(const CXCursor &cursor, CXCursorKind kind, const Location &loc);
    void handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location);
    Location findByUSR(const CXCursor &cursor, CXCursorKind kind, const Location &loc) const;
    void addOverriddenCursors(const CXCursor& cursor, const Location& location, List<CursorInfo*>& infos);

    unsigned mFlags;
    const time_t mTimeStamp;
    bool mIsPch;

    List<Cursor> mDelayed;
    SymbolMap mSymbols;
    SymbolNameMap mSymbolNames;

    enum PathState {
        Unset,
        Index,
        DontIndex,
        Reference
    };
    Map<uint32_t, PathState> mPaths;
    Map<Str, Location> mHeaderMap;
    ReferenceMap mReferences;
    const Path mIn;
    const uint32_t mFileId;
    const List<ByteArray> mArgs;
    DependencyMap mDependencies;
    Set<uint32_t> mPchDependencies;
    Indexer *mIndexer;

    Map<Path, Path> mPchHeaders;
    CXTranslationUnit mUnit;

    ByteArray mMessage;

    Map<ByteArray, uint32_t> mFileIds;

    signalslot::Signal1<IndexerJob*> mFinished;
};

#endif
