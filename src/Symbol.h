#ifndef RTagsCursor_h
#define RTagsCursor_h

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
#include <rct/Serializer.h>

class Project;
struct Symbol
{
    Symbol()
        : symbolLength(0), kind(CXCursor_FirstInvalid), type(CXType_Invalid), enumValue(0),
          startLine(-1), startColumn(-1), endLine(-1), endColumn(-1)
    {}

    Location location;
    String symbolName, usr;
    uint16_t symbolLength;
    CXCursorKind kind;
    CXTypeKind type;
    union {
        bool definition;
        int64_t enumValue; // only used if type == CXCursor_EnumConstantDecl
    };
    int startLine, startColumn, endLine, endColumn;

    bool isNull() const { return !symbolLength; }

    uint16_t targetsValue() const;
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
    bool isReference() const;
    bool isContainer() const;

    inline bool isDefinition() const { return kind == CXCursor_EnumConstantDecl || definition; }

    enum Flag {
        IgnoreTargets = 0x1,
        IgnoreReferences = 0x2,
        DefaultFlags = 0x0
    };
    String toString(unsigned cursorInfoFlags = DefaultFlags,
                    unsigned keyFlags = 0,
                    const std::shared_ptr<Project> &project = std::shared_ptr<Project>()) const;
    String kindSpelling() const { return kindSpelling(kind); }
    String displayName() const;
    static String kindSpelling(uint16_t kind);

    bool operator<(const Symbol &other) const { return location < other.location; }
};


template <> inline Serializer &operator<<(Serializer &s, const Symbol &t)
{
    s << t.location << t.symbolName << t.usr << t.symbolLength
      << static_cast<uint16_t>(t.kind) << static_cast<uint16_t>(t.type)
      << t.enumValue << t.startLine << t.startColumn << t.endLine << t.endColumn;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Symbol &t)
{
    uint16_t kind, type;
    s >> t.location >> t.symbolName >> t.usr >> t.symbolLength
      >> kind >> type >> t.enumValue >> t.startLine >> t.startColumn
      >> t.endLine >> t.endColumn;

    t.kind = static_cast<CXCursorKind>(kind);
    t.type = static_cast<CXTypeKind>(type);
    return s;
}

static inline Log operator<<(Log dbg, const Symbol &symbol)
{
    const String out = "Symbol(" + symbol.toString() + ")";
    return (dbg << out);
}



#endif
