/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

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
        : IndexData(ClangType), unit(0), parseTime(0), visitTime(0)
    {}

    virtual ~IndexDataClang()
    {
        clear();
    }

    void clear()
    {
        if (unit) {
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }
        parseTime = visitTime = 0;
    }
    CXTranslationUnit unit;
    int parseTime, visitTime;
};

class IndexerJobClang : public IndexerJob
{
public:
    IndexerJobClang(const std::shared_ptr<Project> &project, Type type,
                    const SourceInformation &sourceInformation);
    IndexerJobClang(const QueryMessage &msg,
                    const std::shared_ptr<Project> &project,
                    const SourceInformation &sourceInformation);
    static String typeName(const CXCursor &cursor);
    virtual std::shared_ptr<IndexData> createIndexData() { return std::shared_ptr<IndexData>(new IndexDataClang); }

    std::shared_ptr<IndexDataClang> data() const { return std::static_pointer_cast<IndexDataClang>(IndexerJob::data()); }
    String contents() const { return mContents; }
private:
    virtual void index();

    bool diagnose();
    bool visit();
    bool parse();
    void addFileSymbol(uint32_t file);
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

    String mClangLine;
    CXCursor mLastCursor;
    String mContents;
};

#endif
