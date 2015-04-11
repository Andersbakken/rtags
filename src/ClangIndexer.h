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

#include <rct/StopWatch.h>
#include <rct/Hash.h>
#include <rct/Serializer.h>
#include <rct/Path.h>
#include <rct/Connection.h>
#include <sys/stat.h>
#include "IndexDataMessage.h"
#include "IndexerJob.h"
#include "RTagsClang.h"
#include "Symbol.h"

struct Unit;
class ClangIndexer
{
public:
    ClangIndexer();
    ~ClangIndexer();

    bool exec(const String &data);
    static uint32_t serverOpts() { return sServerOpts; }
private:
    bool diagnose();
    bool visit();
    bool parse();
    bool writeFiles(const Path &root, String &error);

    void addFileSymbol(uint32_t file);
    int symbolLength(CXCursorKind kind, const CXCursor &cursor);
    inline Location createLocation(const CXSourceLocation &location, bool *blocked = 0)
    {
        CXString fileName;
        unsigned int line, col;
        CXFile file;
        clang_getSpellingLocation(location, &file, &line, &col, 0);
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
        if (!strcmp(fn, mLastFile.constData())) {
            clang_disposeString(fileName);
            if (mLastBlocked && blocked) {
                *blocked = true;
                return Location();
            } else if (blocked) {
                *blocked = false;
            }

            return Location(mLastFileId, line, col);
        }
        const Path path = RTags::eatString(fileName);
        const Location ret = createLocation(path, line, col, blocked);
        if (blocked) {
            mLastBlocked = *blocked;
            mLastFileId = ret.fileId();
            mLastFile = path;
        }
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
    inline Location createLocation(const CXCursor &cursor, bool *blocked = 0)
    {
        const CXSourceRange range = clang_Cursor_getSpellingNameRange(cursor, 0, 0);
        if (clang_Range_isNull(range))
            return Location();
        return createLocation(clang_getRangeStart(range), blocked);
    }
    Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = 0);
    String addNamePermutations(const CXCursor &cursor, const Location &location,
                               String typeOverride, RTags::CursorType cursorType);

    bool handleCursor(const CXCursor &cursor, CXCursorKind kind,
                      const Location &location, Symbol **cursorPtr = 0);
    bool handleReference(const CXCursor &cursor, CXCursorKind kind,
                         const Location &loc, CXCursor reference,
                         const CXCursor &parent, Symbol **cursorPtr = 0);
    void handleBaseClassSpecifier(const CXCursor &cursor);
    void handleInclude(const CXCursor &cursor, CXCursorKind kind, const Location &location);
    Location findByUSR(const CXCursor &cursor, CXCursorKind kind, const Location &loc) const;
    void addOverriddenCursors(const CXCursor &cursor, const Location &location);
    bool superclassTemplateMemberFunctionUgleHack(const CXCursor &cursor, CXCursorKind kind,
                                                  const Location &location, const CXCursor &ref,
                                                  const CXCursor &parent, Symbol **cursorPtr = 0);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static CXChildVisitResult verboseVisitor(CXCursor cursor, CXCursor, CXClientData userData);
    static CXChildVisitResult resolveAutoTypeRefVisitor(CXCursor cursor, CXCursor, CXClientData data);

    void onMessage(const std::shared_ptr<Message> &msg, const std::shared_ptr<Connection> &conn);

    struct Unit {
        Map<Location, Symbol> symbols;
        Map<Location, Map<String, uint16_t> > targets;
        Map<String, Set<Location> > usrs;
        Map<String, Set<Location> > symbolNames;
    };

    std::shared_ptr<Unit> unit(uint32_t fileId)
    {
        std::shared_ptr<Unit> &unit = mUnits[fileId];
        if (!unit)
            unit.reset(new Unit);
        return unit;
    }
    std::shared_ptr<Unit> unit(const Location &loc) { return unit(loc.fileId()); }

    Symbol findSymbol(const Location &location, bool *ok) const;

    Hash<uint32_t, std::shared_ptr<Unit> > mUnits;

    Path mProject;
    Source mSource;
    Path mSourceFile;
    IndexDataMessage mIndexDataMessage;
    CXTranslationUnit mClangUnit;
    CXIndex mIndex;
    CXCursor mLastCursor;
    Location mLastLocation, mLastClass;
    String mClangLine;
    uint32_t mVisitFileResponseMessageFileId;
    bool mVisitFileResponseMessageVisit;
    Path mSocketFile;
    StopWatch mTimer;
    int mParseDuration, mVisitDuration, mBlocked, mAllowed,
        mIndexed, mVisitFileTimeout, mIndexDataMessageTimeout, mFileIdsQueried;
    UnsavedFiles mUnsavedFiles;
    FILE *mLogFile;
    std::shared_ptr<Connection> mConnection;
    uint32_t mLastFileId;
    bool mLastBlocked;
    Path mLastFile;

    static uint32_t sServerOpts;
};

#endif
