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

    static std::shared_ptr<AST> create(const Source &source, const String &sourceCode, CXTranslationUnit unit);
    List<String> evaluate(const String &script);
    Cursor *root() const { return mRoot; }
    List<Diagnostic> diagnostics() const;
    List<SkippedRange> skippedRanges() const;
    static SourceLocation createLocation(const CXCursor &cursor) { return createLocation(clang_getCursorLocation(cursor)); }
    static SourceLocation createLocation(const CXSourceLocation &location)
    {
        SourceLocation loc;
        loc.mLocation = RTags::createLocation(location, &loc.mOffset);
        return loc;
    }

    Cursor create(const CXCursor &cursor) const
    {
        if (clang_isInvalid(clang_getCursorKind(cursor)))
            return Cursor();

        auto match = [&cursor](const std::vector<Cursor> &cursors) -> Cursor {
            for (const Cursor &c : cursors) {
                assert(c.data);
                if (clang_equalCursors(c.data->cursor, cursor)) {
                    return c;
                }
            }
            // The explicit const cast is here to satisfy travis clang matrix.
            // clang 3.4 does not allow to use two different return types
            return Cursor();
        };

        const std::string usr = toString(clang_getCursorUSR(cursor));
        if (!usr.empty()) {
            auto it = mByUsr.find(usr);
            if (it != mByUsr.end()) {
                const Cursor ret = match(it->second);
                if (ret.data)
                    return ret;
            }
        }

        const SourceLocation loc = createLocation(cursor);
        if (!loc.isNull()) {
            auto it = mByLocation.find(loc);
            if (it != mByLocation.end()) {
                const Cursor ret = match(it->second);
                if (ret.data)
                    return ret;
            }
        }
        return construct(cursor, nullptr, loc, usr);
    }
private:
    static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData u);
    Cursor construct(const CXCursor &cursor,
                     Cursor::Data *parent = nullptr,
                     SourceLocation loc = SourceLocation(),
                     std::string usr = std::string()) const;
    AST()
    {}
    mutable Hash<std::string, std::vector<Cursor>> mByUsr;
    mutable Map<SourceLocation, std::vector<Cursor>> mByLocation;
    String mSourceCode;
    List<String> mReturnValues;
    Cursor *mRoot { nullptr };
};

#endif
