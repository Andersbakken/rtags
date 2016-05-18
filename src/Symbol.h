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

#ifndef RTagsCursor_h
#define RTagsCursor_h

#include <clang-c/Index.h>
#include <memory>
#include <stdint.h>

#include "Location.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Serializer.h"
#include "rct/String.h"

class Project;
struct Symbol
{
    Symbol()
        : symbolLength(0), kind(CXCursor_FirstInvalid), type(CXType_Invalid), linkage(CXLinkage_Invalid),
          flags(None), enumValue(0), startLine(-1), endLine(-1), startColumn(-1), endColumn(-1),
          size(-1), fieldOffset(-1), alignment(-1)
    {}

    Location location;
    String symbolName, usr, typeName;
    List<String> baseClasses;
    List<std::pair<Location, int> > arguments;
    uint16_t symbolLength;
    CXCursorKind kind;
    CXTypeKind type;
    CXLinkageKind linkage;
    enum Flag {
        None = 0x000,
        VirtualMethod = 0x001,
        PureVirtualMethod = 0x002|VirtualMethod,
        StaticMethod = 0x004,
        ConstMethod = 0x008,
        Variadic = 0x010,
        Auto = 0x020,
        AutoRef = 0x040,
        MacroExpansion = 0x080,
        TemplateSpecialization = 0x100,
        InlineFunction = 0x200,
        ImplicitDestruction = 0x400,
        Definition = 0x800
    };
    String briefComment, xmlComment;
    uint16_t flags;
    union {
        int32_t stackCost; // cost for function definitions
        int64_t enumValue; // only used if type == CXCursor_EnumConstantDecl
    };
    int32_t startLine, endLine;
    int16_t startColumn, endColumn;
    int32_t size; // sizeof
    int16_t fieldOffset, alignment; // bits

    bool isNull() const { return location.isNull() || clang_isInvalid(kind); }
    void clear()
    {
        location.clear();
        symbolName.clear();
        usr.clear();
        typeName.clear();
        baseClasses.clear();
        arguments.clear();
        symbolLength = 0;
        kind = CXCursor_FirstInvalid;
        type = CXType_Invalid;
        enumValue = 0;
        flags = 0;
        briefComment.clear();
        xmlComment.clear();
        startLine = startColumn = endLine = endColumn = size = fieldOffset = alignment = -1;
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

    inline bool isDefinition() const { return flags & Definition; }

    enum ToStringFlag {
        DefaultFlags = 0x0,
        IgnoreTargets = 0x1,
        IgnoreReferences = 0x2,
        IgnoreParents = 0x4
    };
    String toString(Flags<ToStringFlag> toStringFlags = DefaultFlags,
                    Flags<Location::ToStringFlag> = Flags<Location::ToStringFlag>(),
                    const std::shared_ptr<Project> &project = std::shared_ptr<Project>()) const;
    String kindSpelling() const { return kindSpelling(kind); }
    String displayName() const;
    static String kindSpelling(uint16_t kind);

    bool operator<(const Symbol &other) const { return location < other.location; }
};

RCT_FLAGS(Symbol::ToStringFlag);

template <> inline Serializer &operator<<(Serializer &s, const Symbol &t)
{
    // SBROOT -- need to find and replace to relative path
    //     symbolName, usr, briefComment, xmlComment
    String tsymbolName = Location::replaceFullWithRelativePath(t.symbolName);
    String tusr = Location::replaceFullWithRelativePath(t.usr);
    String ttypeName = Location::replaceFullWithRelativePath(t.typeName);
    String tbriefComment = Location::replaceFullWithRelativePath(t.briefComment);
    String txmlComment = Location::replaceFullWithRelativePath(t.xmlComment);
    
    s << t.location << tsymbolName << tusr << ttypeName << t.baseClasses << t.arguments
      << t.symbolLength << static_cast<uint16_t>(t.kind) << static_cast<uint16_t>(t.type)
      << static_cast<uint8_t>(t.linkage) << t.flags << tbriefComment << txmlComment
      << t.enumValue << t.startLine << t.endLine << t.startColumn << t.endColumn
      << t.size << t.fieldOffset << t.alignment;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Symbol &t)
{
    uint16_t kind, type;
    uint8_t linkage;
    // SBROOT -- need to find and replace to full path
    s >> t.location >> t.symbolName >> t.usr >> t.typeName >> t.baseClasses
      >> t.arguments >> t.symbolLength >> kind >> type >> linkage >> t.flags
      >> t.briefComment >> t.xmlComment >> t.enumValue
      >> t.startLine >> t.endLine >> t.startColumn >> t.endColumn
      >> t.size >> t.fieldOffset >> t.alignment;

    t.symbolName = Location::replaceRelativeWithFullPath(t.symbolName);
    t.usr = Location::replaceRelativeWithFullPath(t.usr);
    t.typeName = Location::replaceRelativeWithFullPath(t.typeName);
    t.briefComment = Location::replaceRelativeWithFullPath(t.briefComment);
    t.xmlComment = Location::replaceRelativeWithFullPath(t.xmlComment);

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
