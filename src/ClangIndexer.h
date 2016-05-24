/* This file is part of RTags (http://rtags.net).

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

#ifndef ClangIndexer_h
#define ClangIndexer_h

#include <sys/stat.h>
#include "Token.h"

#include "IndexDataMessage.h"
#include "rct/Hash.h"
#include "rct/Path.h"
#include "rct/StopWatch.h"
#include "RTags.h"
#include "Server.h"
#include "Symbol.h"

struct Unit;
class ClangIndexer
{
public:
    static const CXSourceLocation nullLocation;
    static const CXCursor nullCursor;

    ClangIndexer();
    ~ClangIndexer();

    bool exec(const String &data);
    static Flags<Server::Option> serverOpts() { return sServerOpts; }
    static const Path &serverSandboxRoot() { return sServerSandboxRoot; }
private:
    bool diagnose();
    bool visit();
    bool parse();
    void tokenize(CXFile file, uint32_t fileId, const Path &path);
    bool writeFiles(const Path &root, String &error);

    void addFileSymbol(uint32_t file);
    int symbolLength(CXCursorKind kind, const CXCursor &cursor);
    void extractArguments(List<std::pair<Location, int> > *arguments, const CXCursor &cursor);

    inline Location createLocation(const CXSourceLocation &location, bool *blocked = 0, unsigned *offset = 0)
    {
        CXString fileName;
        unsigned int line, col;
        CXFile file;
        clang_getSpellingLocation(location, &file, &line, &col, offset);
        if (file) {
            fileName = clang_getFileName(file);
        } else {
            if (blocked)
                *blocked = false;
            return Location();
        }
        const char *fn = clang_getCString(fileName);
        assert(fn);
        if (!*fn || !strcmp("<built-in>", fn) || !strcmp("<command line>", fn)) {
            if (blocked)
                *blocked = false;
            clang_disposeString(fileName);
            return Location();
        }
        const Path path = RTags::eatString(fileName);
        const Location ret = createLocation(path, line, col, blocked);
        return ret;
    }
    Location createLocation(CXFile file, unsigned int line, unsigned int col, bool *blocked = 0)
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
        return createLocation(p, line, col, blocked);
    }
    inline Location createLocation(const CXCursor &cursor, bool *blocked = 0, unsigned *offset = 0)
    {
        const CXSourceLocation location = clang_getCursorLocation(cursor);
        if (clang_equalLocations(location, nullLocation))
            return Location();
        return createLocation(location, blocked, offset);
    }
    Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = 0);
    String addNamePermutations(const CXCursor &cursor,
                               Location location,
                               RTags::CursorType cursorType);

    CXChildVisitResult handleCursor(const CXCursor &cursor, CXCursorKind kind,
                                    Location location, Symbol **cursorPtr = 0);
    bool handleReference(const CXCursor &cursor, CXCursorKind kind,
                         Location loc, CXCursor reference,
                         Symbol **cursorPtr = 0);
    void handleBaseClassSpecifier(const CXCursor &cursor);
    void handleInclude(const CXCursor &cursor, CXCursorKind kind, Location location);
    void handleLiteral(const CXCursor &cursor, CXCursorKind kind, Location location);
    CXChildVisitResult handleStatement(const CXCursor &cursor, CXCursorKind kind, Location location);
    Location findByUSR(const CXCursor &cursor, CXCursorKind kind, Location loc) const;
    void addOverriddenCursors(const CXCursor &cursor, Location location);
    bool superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                  Location location, const CXCursor &ref,
                                                  Symbol **cursorPtr = 0);
    void visit(CXCursor cursor)
    {
        mParents.append(cursor);
        clang_visitChildren(cursor, visitorHelper, this);
        mParents.removeLast();
    }
    CXChildVisitResult indexVisitor(CXCursor cursor);
    static CXChildVisitResult visitorHelper(CXCursor cursor, CXCursor, CXClientData userData);
    static CXChildVisitResult verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData);
    static CXChildVisitResult resolveAutoTypeRefVisitor(CXCursor cursor, CXCursor, CXClientData data);

    void onMessage(const std::shared_ptr<Message> &msg, const std::shared_ptr<Connection> &conn);

    struct Unit {
        Map<Location, Symbol> symbols;
        Map<Location, Map<String, uint16_t> > targets;
        Map<String, Set<Location> > usrs;
        Map<String, Set<Location> > symbolNames;
        Map<uint32_t, Token> tokens;
    };

    std::shared_ptr<Unit> unit(uint32_t fileId)
    {
        std::shared_ptr<Unit> &unit = mUnits[fileId];
        if (!unit)
            unit.reset(new Unit);
        return unit;
    }
    std::shared_ptr<Unit> unit(Location loc) { return unit(loc.fileId()); }

    enum FindResult {
        Found,
        NotIndexed,
        NotFound
    };
    Symbol findSymbol(Location location, FindResult *result) const;

    struct MacroLocationData {
        Set<size_t> arguments;
        List<Location> locations;
    };
    struct MacroData {
        List<String> arguments;
        Map<String, MacroLocationData> data;
    };
    Map<Location, MacroData> mMacroTokens;

    Hash<uint32_t, std::shared_ptr<Unit> > mUnits;

    Path mProject;
    Source mSource;
    Path mSourceFile;
    IndexDataMessage mIndexDataMessage;
    std::shared_ptr<RTags::TranslationUnit> mTranslationUnit;
    CXCursor mLastCursor;
    Symbol *mLastCallExprSymbol;
    Location mLastClass;
    uint32_t mVisitFileResponseMessageFileId;
    bool mVisitFileResponseMessageVisit;
    Path mSocketFile;
    StopWatch mTimer;
    int mParseDuration, mVisitDuration, mBlocked, mAllowed,
        mIndexed, mVisitFileTimeout, mIndexDataMessageTimeout,
        mFileIdsQueried, mFileIdsQueriedTime, mCursorsVisited;
    UnsavedFiles mUnsavedFiles;
    List<String> mDebugLocations;
    FILE *mLogFile;
    std::shared_ptr<Connection> mConnection;
    Path mDataDir;
    bool mUnionRecursion;

    struct Scope {
        enum ScopeType {
            FunctionDefinition,
            FunctionDeclaration,
            Other
        };
        ScopeType type;
        Symbol *symbol;
        Location start, end;
    };
    List<Scope> mScopeStack;

    struct Loop { // or switch
        CXCursorKind kind;
        Location start, end;
    };
    List<Loop> mLoopStack;

    List<CXCursor> mParents;

    static Flags<Server::Option> sServerOpts;
    static Path sServerSandboxRoot;
};

#endif
