#ifndef CursorInfo_h
#define CursorInfo_h

#include <ByteArray.h>
#include <clang-c/Index.h>
#include <Path.h>
#include <RTags.h>
#include "Location.h"
#include "Rdm.h"

class CursorInfo
{
public:
    CursorInfo()
        : symbolLength(0), kind(CXCursor_FirstInvalid), isDefinition(false)
    {}
    bool isNull() const { return !symbolLength; }
    bool isValid() const { return symbolLength; }
    void clear()
    {
        symbolLength = 0;
        kind = CXCursor_FirstInvalid;
        isDefinition = false;
        target.clear();
        references.clear();
        symbolName.clear();
    }

    bool dirty(const Set<uint32_t> &fileIds, bool selfDirty)
    {
        bool changed = selfDirty;
        if (selfDirty) {
            symbolLength = 0;
            kind = CXCursor_FirstInvalid;
            isDefinition = false;
            symbolName.clear();
        }

        if (fileIds.contains(target.fileId())) {
            changed = true;
            target.clear();
        }

        Set<Location>::iterator it = references.begin();
        while (it != references.end()) {
            if (fileIds.contains((*it).fileId())) {
                changed = true;
                references.erase(it++);
            } else {
                ++it;
            }
        }
        return changed;
    }

    bool unite(const CursorInfo &other)
    {
        bool changed = false;
        if (target.isNull() && !other.target.isNull()) {
            target = other.target;
            changed = true;
        }

        // ### this is not ideal, we can probably know this rather than check all of them
        if (symbolName.isEmpty() && !other.symbolName.isEmpty()) {
            symbolName = other.symbolName;
            changed = true;
        }

        if (kind == CXCursor_FirstInvalid) {
            kind = other.kind;
            changed = true;
        }
        isDefinition = isDefinition || other.isDefinition; // ### hm

        if (!symbolLength && other.symbolLength) {
            symbolLength = other.symbolLength;
            changed = true;
        }
        const int oldSize = references.size();
        if (!oldSize) {
            references = other.references;
            if (!other.references.isEmpty())
                changed = true;
        } else {
            references.unite(other.references);
            if (oldSize != references.size())
                changed = true;
        }
        return changed;
    }


    int symbolLength; // this is just the symbol name e.g. foo
    ByteArray symbolName; // this is fully qualified Foobar::Barfoo::foo
    CXCursorKind kind;
    bool isDefinition;
    Location target;
    Set<Location> references;
};

#endif
