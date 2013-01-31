#ifndef IndexerJob_h
#define IndexerJob_h

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
    String message;
    UsrMap usrMap;
    FixItMap fixIts;
    DiagnosticsMap diagnostics;
};

typedef List<std::pair<CXIndex, CXTranslationUnit> > UnitList;

class IndexerJob : public Job
{
public:
    enum Flag {
        Makefile = 0x1,
        Dirty = 0x2,
        IgnorePrintfFixits = 0x4
    };
    IndexerJob(const shared_ptr<Project> &project, unsigned flags,
               const SourceInformation &sourceInformation);
    IndexerJob(const QueryMessage &msg,
               const shared_ptr<Project> &project,
               const SourceInformation &sourceInformation);

    shared_ptr<IndexData> data() const { return mData; }
    uint32_t fileId() const { return mFileId; }
    Path path() const { return mSourceInformation.sourceFile; }
    bool abortIfStarted();
    const SourceInformation &sourceInformation() const { return mSourceInformation; }
    time_t parseTime() const { return mParseTime; }
    const Set<uint32_t> &visitedFiles() const { return mVisitedFiles; }
    unsigned flags() const { return mFlags; }
private:
    bool parse(int build);
    bool visit(int build);
    bool diagnose(int build, int *errorCount);

    virtual void execute();

    Location createLocation(const CXSourceLocation &location, bool *blocked);
    inline Location createLocation(const CXCursor &cursor, bool *blocked)
    {
        return createLocation(clang_getCursorLocation(cursor), blocked);
    }
    static Location createLocation(const CXCursor &cursor);
    String addNamePermutations(const CXCursor &cursor, const Location &location);
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

    Set<uint32_t> mVisitedFiles;
    Set<uint32_t> mBlockedFiles;

    SourceInformation mSourceInformation;
    const uint32_t mFileId;
    UnitList mUnits;

    Map<String, uint32_t> mFileIds;

    List<String> mClangLines;

    StopWatch mTimer;
    shared_ptr<IndexData> mData;

    bool mDump;

    time_t mParseTime;
    bool mStarted;
};

#endif
