/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

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
#include <unordered_set>

struct Unit;
class ClangIndexer : public RTags::DiagnosticsProvider
{
public:
    enum Mode {
        Normal,
        Daemon
    };
    ClangIndexer(Mode mode);
    ~ClangIndexer();

    enum State {
        NotStarted,
        Stopped,
        Running
    };

    static void transition(State state)
    {
        std::unique_lock<std::mutex> lock(sStateMutex);
        sState = state;
    }

    static State state()
    {
        std::unique_lock<std::mutex> lock(sStateMutex);
        return sState;
    }
    Path sourceFile() const { return mSourceFile; }
    bool exec(const String &data);
    static Flags<Server::Option> serverOpts() { return sServerOpts; }
private:
    bool diagnose();
    bool visit();
    bool parse();
    void tokenize(CXFile file, uint32_t fileId, const Path &path);
    bool writeFiles(const Path &root, String &error);

    void addFileSymbol(uint32_t file);
    int symbolLength(CXCursorKind kind, const CXCursor &cursor);
    void extractArguments(List<Symbol::Argument> *arguments, const CXCursor &cursor);
    CXCursor resolveTemplate(CXCursor cursor, Location location = Location(), bool *specialized = nullptr);
    CXCursor resolveTemplateUsr(CXCursor cursor) const;
    static CXCursor resolveTypedef(CXCursor cursor);

    // DiagnosticsProvider
    using RTags::DiagnosticsProvider::createLocation;
    virtual Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = nullptr) override;
    virtual CXTranslationUnit unit(size_t u) const override;
    virtual size_t unitCount() const override
    {
        return mTranslationUnits.size();
    }
    virtual size_t diagnosticCount(size_t unit) const override
    {
        if (CXTranslationUnit u = mTranslationUnits[unit]->unit) {
            return clang_getNumDiagnostics(u);
        }
        return 0;
    }
    virtual CXDiagnostic diagnostic(size_t unit, size_t idx) const override
    {
        assert(mTranslationUnits[unit]->unit);
        return clang_getDiagnostic(mTranslationUnits[unit]->unit, idx);
    }

    virtual uint32_t sourceFileId() const override { return mSources.front().fileId; }
    virtual IndexDataMessage &indexDataMessage() override { return mIndexDataMessage; }

    String addNamePermutations(const CXCursor &cursor,
                               Location location,
                               RTags::CursorType cursorType);

    CXChildVisitResult handleCursor(const CXCursor &cursor, CXCursorKind kind,
                                    Location location, Symbol **cursorPtr = nullptr);
    bool handleReference(const CXCursor &cursor, CXCursorKind kind,
                         Location loc, CXCursor reference,
                         Symbol **cursorPtr = nullptr);
    void handleBaseClassSpecifier(const CXCursor &cursor);
    void handleInclude(const CXCursor &cursor, CXCursorKind kind, Location location);
    void handleLiteral(const CXCursor &cursor, CXCursorKind kind, Location location);
    CXChildVisitResult handleStatement(const CXCursor &cursor, CXCursorKind kind, Location location);
    Location findByUSR(const CXCursor &cursor, CXCursorKind kind, Location loc) const;
    std::unordered_set<CXCursor> addOverriddenCursors(const CXCursor &cursor, Location location);
    bool superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                  Location location, const CXCursor &ref,
                                                  Symbol **cursorPtr = nullptr);
    void handleMakeShared(const CXCursor &cursor, Map<String, uint16_t> &targets);
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

    std::shared_ptr<Unit> &unit(uint32_t fileId)
    {
        std::shared_ptr<Unit> &unit = mUnits[fileId];
        if (!unit) {
            unit.reset(new Unit);
        }
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

    const Mode mMode;
    Path mProject;
    SourceList mSources, mCachedSources;
    Path mSourceFile;
    IndexDataMessage mIndexDataMessage;
    List<std::shared_ptr<RTags::TranslationUnit> > mTranslationUnits, mCachedTranslationUnits;
    size_t mCurrentTranslationUnit;
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
    bool mFromCache;

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
    std::unordered_set<CXCursor> mTemplateSpecializations;
    size_t mInTemplateFunction;

    static Flags<Server::Option> sServerOpts;

    static State sState;
    static std::mutex sStateMutex;
};

#endif
