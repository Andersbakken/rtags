#ifndef CursorInfo_h
#define CursorInfo_h

#include <ByteArray.h>
#include <clang-c/Index.h>
#include <Path.h>
#include <RTags.h>
#include "Location.h"
#include "Rdm.h"

static inline bool match(uint32_t fileId, const Location &loc)
{
    return loc.fileId() == fileId;
}

static inline bool match(const Set<uint32_t> &fileIds, const Location &loc)
{
    return fileIds.contains(loc.fileId());
}


class CursorInfo
{
public:
    CursorInfo()
        : symbolLength(0), kind(CXCursor_FirstInvalid), isDefinition(false)
    {}
    void clear()
    {
        symbolLength = 0;
        kind = CXCursor_FirstInvalid;
        isDefinition = false;
        target.clear();
        references.clear();
        symbolName.clear();
    }

    enum DirtyState {
        Empty = -1,
        Unchanged = 0,
        Modified = 1
    };
    template <typename T>
    DirtyState dirty(T dirty, bool selfDirty)
    {
        bool changed = false;
        if (selfDirty && symbolLength) {
            symbolLength = 0;
            kind = CXCursor_FirstInvalid;
            isDefinition = false;
            symbolName.clear();
            changed = true;
        }

        if (match(dirty, target)) {
            changed = true;
            target.clear();
        }

        Set<Location>::iterator it = references.begin();
        while (it != references.end()) {
            if (match(dirty, it->fileId())) {
                changed = true;
                references.erase(it++);
            } else {
                ++it;
            }
        }
        return changed ? (isEmpty() ? Empty : Modified) : Unchanged;
    }

    bool isEmpty() const
    {
        assert(symbolLength || symbolName.isEmpty() && symbolLength || kind == CXCursor_FirstInvalid); // these should be coupled
        return !symbolLength && !target && references.isEmpty();
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
