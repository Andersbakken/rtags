#ifndef RTagsCursor_h
#define RTagsCursor_h

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
#include <stdint.h>
#include "Location.h"
#include <rct/String.h>
#include <rct/Serializer.h>
#include <rct/Flags.h>

class Project;
struct Symbol
{
    Symbol()
        : symbolLength(0), kind(CXCursor_FirstInvalid), type(CXType_Invalid), linkage(CXLinkage_Invalid),
          flags(None), enumValue(0), startLine(-1), startColumn(-1), endLine(-1), endColumn(-1)
    {}

    Location location;
    String symbolName, usr;
    List<String> baseClasses;
    uint16_t symbolLength;
    CXCursorKind kind;
    CXTypeKind type;
    CXLinkageKind linkage;
    enum Flag {
        None = 0x00,
        VirtualMethod = 0x01,
        PureVirtualMethod = 0x02|VirtualMethod,
        StaticMethod = 0x04,
        ConstMethod = 0x08,
        Variadic = 0x10,
        Auto = 0x20,
        AutoRef = 0x40
    };
    String briefComment, xmlComment;
    uint8_t flags;
    union {
        bool definition;
        int64_t enumValue; // only used if type == CXCursor_EnumConstantDecl
    };
    int startLine, startColumn, endLine, endColumn;

    bool isNull() const { return location.isNull(); }
    void clear()
    {
        location.clear();
        symbolName.clear();
        usr.clear();
        symbolLength = 0;
        kind = CXCursor_FirstInvalid;
        type = CXType_Invalid;
        enumValue = 0;
        flags = 0;
        briefComment.clear();
        xmlComment.clear();
        startLine = startColumn = endLine = endColumn = -1;
    }

    uint16_t targetsValue() const;
    static bool isClass(CXCursorKind kind)
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

    bool isClass() const { return isClass(kind); }
    bool isConstructorOrDestructor() const
    {
        switch (kind) {
        case CXCursor_Constructor:
        case CXCursor_Destructor:
            return true;
        default:
            break;
        }
        return false;
    }
    bool isReference() const;
    bool isContainer() const;

    inline bool isDefinition() const { return kind == CXCursor_EnumConstantDecl || definition; }

    enum ToStringFlag {
        IgnoreTargets = 0x1,
        IgnoreReferences = 0x2,
        DefaultFlags = 0x0
    };
    String toString(Flags<ToStringFlag> toStringFlags = DefaultFlags,
                    Flags<Location::KeyFlag> = Flags<Location::KeyFlag>(),
                    const std::shared_ptr<Project> &project = std::shared_ptr<Project>()) const;
    String kindSpelling() const { return kindSpelling(kind); }
    String displayName() const;
    static String kindSpelling(uint16_t kind);

    bool operator<(const Symbol &other) const { return location < other.location; }
};

RCT_FLAGS(Symbol::ToStringFlag);

template <> inline Serializer &operator<<(Serializer &s, const Symbol &t)
{
    s << t.location << t.symbolName << t.usr << t.baseClasses << t.symbolLength
      << static_cast<uint16_t>(t.kind) << static_cast<uint16_t>(t.type)
      << static_cast<uint8_t>(t.linkage) << t.flags << t.briefComment << t.xmlComment
      << t.enumValue << t.startLine << t.startColumn << t.endLine << t.endColumn;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Symbol &t)
{
    uint16_t kind, type;
    uint8_t linkage;
    s >> t.location >> t.symbolName >> t.usr >> t.baseClasses
      >> t.symbolLength >> kind >> type >> linkage >> t.flags
      >> t.briefComment >> t.xmlComment >> t.enumValue
      >> t.startLine >> t.startColumn >> t.endLine >> t.endColumn;

    t.kind = static_cast<CXCursorKind>(kind);
    t.type = static_cast<CXTypeKind>(type);
    t.linkage = static_cast<CXLinkageKind>(linkage);
    return s;
}

static inline Log operator<<(Log dbg, const Symbol &symbol)
{
    const String out = "Symbol(" + symbol.toString() + ")";
    return (dbg << out);
}



#endif
