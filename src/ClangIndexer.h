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
#include <unordered_set>

struct Unit;
class ClangIndexer : public RTags::DiagnosticsProvider
{
public:
    ClangIndexer();
    ~ClangIndexer();

    struct Config {
        Path sourceFile;
        SourceList sources;
        Path project;
        Path dataDir;
        UnsavedFiles unsavedFiles;
        Flags<IndexerJob::Flag> indexerJobFlags;
        Flags<Server::Option> serverOpts;
        List<String> debugLocations;
        uint64_t id { 0 };
    };

    bool exec(Config &&config);
    const Path &sourceFile() const { return mSourceFile; }
    const Path &project() const { return mProject; }
    const SourceList &sources() const { return mSources; }
    const CXCursor &lastCursor() const { return mLastCursor; }
    int &indexed() { return mIndexed; } // ### ugh
    int &fileIdsQueried() { return mFileIdsQueried; }
    int &fileIdsQueriedTime() { return mFileIdsQueriedTime; }
protected:
    bool diagnose();
    bool visit();
    bool parse();
    virtual bool send(const IndexDataMessage &message) = 0;
    virtual IndexDataMessage &indexDataMessage() override { return mIndexDataMessage; }
private:
    bool writeFiles(const Path &root, String &error);
    void tokenize(CXFile file, uint32_t fileId, const Path &path);

    void addFileSymbol(uint32_t file);
    int symbolLength(CXCursorKind kind, const CXCursor &cursor);
    void extractArguments(List<Symbol::Argument> *arguments, const CXCursor &cursor);
    CXCursor resolveTemplate(CXCursor cursor, Location location = Location(), bool *specialized = 0);
    static CXCursor resolveTypedef(CXCursor cursor);

    // DiagnosticsProvider
    using RTags::DiagnosticsProvider::createLocation;
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
    std::unordered_set<CXCursor> addOverriddenCursors(const CXCursor &cursor, Location location);
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

    Path mProject;
    SourceList mSources;
    Path mSourceFile;
    IndexDataMessage mIndexDataMessage;
    List<std::shared_ptr<RTags::TranslationUnit> > mTranslationUnits;
    size_t mCurrentTranslationUnit { String::npos };
    CXCursor mLastCursor { clang_getNullCursor() };
    Symbol *mLastCallExprSymbol { nullptr };
    Location mLastClass;
    Flags<Server::Option> mServerOpts;

    uint32_t mVisitFileResponseMessageFileId;
    bool mVisitFileResponseMessageVisit;
    Path mSocketFile;
    StopWatch mTimer;
    int mParseDuration { 0 }, mVisitDuration { 0 }, mBlocked { 0 }, mAllowed { 0 },
        mIndexed { 1 }, mCursorsVisited { 0 },
        mFileIdsQueried { 0 }, mFileIdsQueriedTime { 0 };

    UnsavedFiles mUnsavedFiles;
    List<String> mDebugLocations;
    FILE *mLogFile { nullptr };
    Path mDataDir;
    bool mUnionRecursion { false };

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

    std::shared_ptr<RTags::TranslationUnit> mSerializeTU;
    List<CXCursor> mParents;
    std::unordered_set<CXCursor> mTemplateSpecializations;
    size_t mInTemplateFunction { 0 };
};

#endif
