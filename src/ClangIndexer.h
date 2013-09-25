#ifndef ClangIndexer_h
#define ClangIndexer_h

#include <rct/StopWatch.h>
#include <rct/Hash.h>
#include <rct/Serializer.h>
#include <rct/Path.h>
#include <rct/Connection.h>
#include "IndexerJob.h"
#include "RTagsClang.h"

class ClangIndexer
{
public:
    ClangIndexer();
    ~ClangIndexer();

    bool connect(const Path &serverFile);
    bool index(IndexType type, uint64_t id, const Path &project, uint32_t fileId,
               const Path &sourceFile, const List<String> &args);
private:
    bool diagnose();
    bool visit();
    bool parse();

    void addFileSymbol(uint32_t file);
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
    Location createLocation(CXFile file, unsigned offset, bool *blocked = 0)
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
        return createLocation(p, offset, blocked);
    }
    inline Location createLocation(const CXCursor &cursor, bool *blocked = 0)
    {
        return createLocation(clang_getCursorLocation(cursor), blocked);
    }
    Location createLocation(const Path &file, unsigned start, bool *blocked);
    String addNamePermutations(const CXCursor &cursor, const Location &location);

    bool handleCursor(const CXCursor &cursor, CXCursorKind kind, const Location &location, bool a);
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
    static CXChildVisitResult dumpVisitor(CXCursor cursor, CXCursor, CXClientData userData);

    static void inclusionVisitor(CXFile includedFile, CXSourceLocation *includeStack,
                                 unsigned includeLen, CXClientData userData);

    static String typeName(const CXCursor &cursor);
    static String typeString(const CXType &type);

    Path mProject;
    std::shared_ptr<IndexData> mData;
    String mContents;
    List<String> mArgs;
    CXTranslationUnit mUnit;
    CXIndex mIndex;
    CXCursor mLastCursor;
    String mClangLine;
    // Hash<uint32_t, int> mErrors;
    int mVisitedFiles;
    Path mSocketFile;
    StopWatch mTimer;
    int mParseDuration, mVisitDuration, mCommunicationDuration, mBlocked, mAllowed;
    Connection mConnection;
    FILE *mLogFile;
};

#endif
