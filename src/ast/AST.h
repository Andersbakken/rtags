#ifndef AST_h
#define AST_h

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

#include <clang-c/Index.h>
#include "Source.h"
#include "Location.h"
#include "RTags.h"
#include "rct/String.h"
#include "rct/List.h"
#include "SourceLocation.h"
#include "SourceRange.h"
#include "CursorType.h"
#include "Cursor.h"

class AST
{
public:
    template <typename T>
    static std::string toString(T t)
    {
        String str;
        Log(&str) << t;
        return str.ref();
    }
    static std::string toString(CXString str)
    {
        return RTags::eatString(str).ref();
    }
    struct Diagnostic {


    };

    struct SkippedRange {


    };

    static std::shared_ptr<AST> create(const Source &source,
                                       const String &sourceCode,
                                       CXTranslationUnit unit,
                                       const std::vector<std::pair<Path, String> > &scripts,
                                       const std::function<void(const String &)> &outputHandler);
    // Cursor *root() const { return mRoot; }
    // List<Diagnostic> diagnostics() const;
    // List<SkippedRange> skippedRanges() const;
    static std::shared_ptr<SourceLocation> createLocation(const CXCursor &cursor) { return createLocation(clang_getCursorLocation(cursor)); }
    static std::shared_ptr<SourceLocation> createLocation(const CXSourceLocation &location)
    {
        std::shared_ptr<SourceLocation> loc = std::make_shared<SourceLocation>();
        loc->mLocation = RTags::createLocation(location, &loc->mOffset);
        return loc;
    }

    std::shared_ptr<Cursor> create(const CXCursor &cursor) const
    {
        if (clang_isInvalid(clang_getCursorKind(cursor)))
            return nullptr;

        auto match = [&cursor](const std::vector<std::shared_ptr<Cursor> > &cursors) -> std::shared_ptr<Cursor> {
            for (const std::shared_ptr<Cursor> &c : cursors) {
                if (clang_equalCursors(c->mCursor, cursor)) {
                    return c;
                }
            }
            return std::shared_ptr<Cursor>();
        };

        const std::string usr = toString(clang_getCursorUSR(cursor));
        if (!usr.empty()) {
            auto it = mByUsr.find(usr);
            if (it != mByUsr.end()) {
                const std::shared_ptr<Cursor> ret = match(it->second);
                if (ret)
                    return ret;
            }
        }

        const std::shared_ptr<SourceLocation> loc = createLocation(cursor);
        if (!loc->isNull()) {
            auto it = mByLocation.find(*loc);
            if (it != mByLocation.end()) {
                const std::shared_ptr<Cursor> ret = match(it->second);
                if (ret)
                    return ret;
            }
        }
        return construct(cursor, nullptr, loc, usr);
    }
    String &currentOutput() { return mCurrentOutput; }
private:
    static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData u);
    std::shared_ptr<Cursor>  construct(const CXCursor &cursor,
                                       const std::shared_ptr<Cursor> &parent = nullptr,
                                       std::shared_ptr<SourceLocation> loc = nullptr,
                                       std::string usr = std::string()) const;
    AST()
    {}
    mutable Hash<std::string, std::vector<std::shared_ptr<Cursor> > > mByUsr;
    mutable Map<SourceLocation, std::vector<std::shared_ptr<Cursor> > > mByLocation;
    String mCurrentOutput;
};

#endif
