#ifndef Cursor_h
#define Cursor_h

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

#include <clang-c/Index.h>
#include <stdint.h>
#include "Location.h"
#include <rct/String.h>

struct Cursor
{
    Cursor()
        : symbolLength(0), kind(CXCursor_FirstInvalid), type(CXType_Invalid), enumValue(0)
    {}

    Location location;
    String symbolName;
    uint16_t symbolLength;
    CXCursorKind kind;
    CXTypeKind type;
    union {
        bool definition;
        int64_t enumValue; // only used if type == CXCursor_EnumConstantDecl
    };
    int32_t startLine, startColumn, endLine, endColumn;

    bool isNull() const { return !symbolLength; }

    enum { DefinitionBit = 0x1000 };
    static CXCursorKind targetsValueKind(uint16_t val)
    {
        return static_cast<CXCursorKind>(val & ~DefinitionBit);
    }
    static bool targetsValueIsDefinition(uint16_t val)
    {
        return val & DefinitionBit;
    }
    static uint16_t createTargetsValue(CXCursorKind kind, bool definition)
    {
        return (kind | (definition ? DefinitionBit : 0));
    }
    static uint16_t createTargetsValue(const CXCursor &cursor)
    {
        return createTargetsValue(clang_getCursorKind(cursor), clang_isCursorDefinition(cursor));
    }

    uint16_t targetsValue() const { return createTargetsValue(kind, isDefinition()); }
    bool isClass() const
    {
        switch (kind) {
        case CXCursor_ClassDecl:
        case CXCursor_ClassTemplate:
        case CXCursor_StructDecl:
            return true;
        default:
            break;
        }
        return false;
    }

    inline bool isDefinition() const { return kind == CXCursor_EnumConstantDecl || definition; }

};

#endif
