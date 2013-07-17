#ifndef IndexerJobClang_h
#define IndexerJobClang_h

#include "IndexerJob.h"
#include "RTagsClang.h"
#include "Str.h"
#include <clang-c/Index.h>

class IndexDataClang : public IndexData
{
public:
    IndexDataClang()
        : IndexData(ClangType), unit(0), index(0)
    {}

    virtual ~IndexDataClang()
    {
        clear();
    }

    void clear()
    {
        if (index) {
            clang_disposeIndex(index);
            index = 0;
        }

        if (unit) {
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }
    }
    CXTranslationUnit unit;
    CXIndex index;
};

class IndexerJobClang : public IndexerJob
{
public:
    IndexerJobClang(const shared_ptr<Project> &project, Type type,
                    const SourceInformation &sourceInformation);
    IndexerJobClang(const QueryMessage &msg,
                    const shared_ptr<Project> &project,
                    const SourceInformation &sourceInformation);
    static String typeName(const CXCursor &cursor);
    virtual shared_ptr<IndexData> createIndexData() { return shared_ptr<IndexData>(new IndexDataClang); }

    shared_ptr<IndexDataClang> data() const { return static_pointer_cast<IndexDataClang>(IndexerJob::data()); }
    String contents() const { return mContents; }
private:
    virtual void index();

    bool diagnose();
    bool visit();
    bool parse();

    using IndexerJob::createLocation;
    inline Location createLocation(const CXSourceLocation &location, bool *blocked)
    {
        if (blocked)
            *blocked = false;

        CXFile file;
        unsigned start;
        clang_getSpellingLocation(location, &file, 0, 0, &start);
        if (file) {
            return createLocation(RTags::eatString(clang_getFileName(file)), start, blocked);
        }
        return Location();
    }
    inline Location createLocation(const CXCursor &cursor, bool *blocked = 0)
    {
        return createLocation(clang_getCursorLocation(cursor), blocked);
    }
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
    void superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                  const Location &location, const CXCursor &ref,
                                                  const CXCursor &parent);
    void nestedClassConstructorCallUgleHack(const CXCursor &parent, CursorInfo &info,
                                            CXCursorKind refKind, const Location &refLoc);

    String mClangLine;
    CXCursor mLastCursor;
    String mContents;
};

#endif
