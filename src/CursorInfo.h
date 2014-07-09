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

#ifndef CursorInfo_h
#define CursorInfo_h

#include <rct/String.h>
#include "Location.h"
#include <rct/Path.h>
#include <rct/Log.h>
#include <rct/List.h>
#include <clang-c/Index.h>

class CursorInfo;
typedef Map<Location, std::shared_ptr<CursorInfo> > SymbolMap;
class CursorInfo
{
public:
    CursorInfo()
        : symbolLength(0), kind(CXCursor_FirstInvalid), type(CXType_Invalid), enumValue(0),
          startLine(-1), startColumn(-1), endLine(-1), endColumn(-1)
    {}

    void clear()
    {
        symbolLength = 0;
        kind = CXCursor_FirstInvalid;
        type = CXType_Invalid;
        enumValue = 0;
        targets.clear();
        references.clear();
        symbolName.clear();
    }

    String kindSpelling() const { return kindSpelling(kind); }
    static String kindSpelling(uint16_t kind);
    bool dirty(const Set<uint32_t> &dirty)
    {
        bool changed = false;
        Set<Location> *locations[] = { &targets, &references };
        for (int i=0; i<2; ++i) {
            Set<Location> &l = *locations[i];
            Set<Location>::iterator it = l.begin();
            while (it != l.end()) {
                if (dirty.contains(it->fileId())) {
                    changed = true;
                    l.erase(it++);
                } else {
                    ++it;
                }
            }
        }
        return changed;
    }

    String displayName() const;

    int targetRank(const std::shared_ptr<CursorInfo> &target) const;

    bool isValid() const
    {
        return !isEmpty();
    }

    bool isNull() const
    {
        return isEmpty();
    }

    std::shared_ptr<CursorInfo> bestTarget(const SymbolMap &map, Location *loc = 0) const;
    SymbolMap targetInfos(const SymbolMap &map) const;
    SymbolMap referenceInfos(const SymbolMap &map) const;
    SymbolMap callers(const Location &loc, const SymbolMap &map) const;
    SymbolMap allReferences(const Location &loc, const SymbolMap &map) const;
    SymbolMap virtuals(const Location &loc, const SymbolMap &map) const;
    SymbolMap declarationAndDefinition(const Location &loc, const SymbolMap &map) const;

    std::shared_ptr<CursorInfo> copy() const;

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

    inline bool isDefinition() const
    {
        return kind == CXCursor_EnumConstantDecl || definition;
    }

    bool isEmpty() const
    {
        return !symbolLength && targets.isEmpty() && references.isEmpty();
    }

    bool unite(const std::shared_ptr<CursorInfo> &other)
    {
        bool changed = false;
        if (targets.isEmpty() && !other->targets.isEmpty()) {
            targets = other->targets;
            changed = true;
        } else if (!other->targets.isEmpty()) {
            int count = 0;
            targets.unite(other->targets, &count);
            if (count)
                changed = true;
        }

        if (startLine == -1 && other->startLine != -1) {
            startLine = other->startLine;
            startColumn = other->startColumn;
            endLine = other->endLine;
            endColumn = other->endColumn;
            changed = true;
        }

        if (!symbolLength && other->symbolLength) {
            symbolLength = other->symbolLength;
            kind = other->kind;
            enumValue = other->enumValue;
            type = other->type;
            symbolName = other->symbolName;
            changed = true;
        }
        const int oldSize = references.size();
        if (!oldSize) {
            references = other->references;
            if (!references.isEmpty())
                changed = true;
        } else {
            int inserted = 0;
            references.unite(other->references, &inserted);
            if (inserted)
                changed = true;
        }

        return changed;
    }

    template <typename T>
    static inline void serialize(T &s, const SymbolMap &t);
    template <typename T>
    static inline void deserialize(T &s, SymbolMap &t);

    enum Flag {
        IgnoreTargets = 0x1,
        IgnoreReferences = 0x2,
        DefaultFlags = 0x0
    };
    String toString(unsigned cursorInfoFlags = DefaultFlags, unsigned keyFlags = 0) const;
    uint16_t symbolLength; // this is just the symbol name length e.g. foo => 3
    String symbolName; // this is fully qualified Foobar::Barfoo::foo
    uint16_t kind;
    CXTypeKind type;
    union {
        bool definition;
        int64_t enumValue; // only used if type == CXCursor_EnumConstantDecl
    };
    Set<Location> targets, references;
    int startLine, startColumn, endLine, endColumn;
};

template <> inline Serializer &operator<<(Serializer &s, const CursorInfo &t)
{
    s << t.symbolLength << t.symbolName << static_cast<int>(t.kind)
      << static_cast<int>(t.type) << t.enumValue << t.targets << t.references
      << t.startLine << t.startColumn << t.endLine << t.endColumn;
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, CursorInfo &t)
{
    int kind, type;
    s >> t.symbolLength >> t.symbolName >> kind >> type
      >> t.enumValue >> t.targets >> t.references
      >> t.startLine >> t.startColumn >> t.endLine >> t.endColumn;
    t.kind = static_cast<CXCursorKind>(kind);
    t.type = static_cast<CXTypeKind>(type);
    return s;
}

template <typename T>
inline void CursorInfo::serialize(T &s, const SymbolMap &t)
{
    const uint32_t size = t.size();
    s << size;
    for (const auto &it : t)
        s << it.first << *it.second;
}

template <typename T>
inline void CursorInfo::deserialize(T &s, SymbolMap &t)
{
    uint32_t size;
    s >> size;
    t.clear();
    while (size--) {
        Location location;
        s >> location;
        std::shared_ptr<CursorInfo> &ci = t[location];
        ci = std::make_shared<CursorInfo>();
        s >> *ci;
    }
}

inline Log operator<<(Log log, const CursorInfo &info)
{
    log << info.toString();
    return log;
}

#endif
