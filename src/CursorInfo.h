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
        : symbolLength(0), kind(CXCursor_FirstInvalid), isDefinition(false), ref(0)
    {}
    void clear()
    {
        symbolLength = 0;
        kind = CXCursor_FirstInvalid;
        isDefinition = false;
        target.clear();
        references.clear();
        symbolName.clear();
        ref = 0;
    }

    enum DirtyState {
        Empty = -1,
        Unchanged = 0,
        Modified = 1
    };
    DirtyState dirty(const Set<uint32_t> &dirty, bool selfDirty)
    {
        bool changed = false;
        if (selfDirty && symbolLength) {
            symbolLength = 0;
            kind = CXCursor_FirstInvalid;
            isDefinition = false;
            symbolName.clear();
            changed = true;
        }

        const uint32_t targetFileId = target.fileId();
        if (targetFileId && dirty.contains(targetFileId)) {
            changed = true;
            target.clear();
        }

        Set<Location>::iterator it = references.begin();
        while (it != references.end()) {
            if (dirty.contains(it->fileId())) {
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
        assert((symbolLength || symbolName.isEmpty()) && (symbolLength || kind == CXCursor_FirstInvalid)); // these should be coupled
        return !symbolLength && !target && references.isEmpty();
    }

    bool unite(const CursorInfo &other)
    {
        bool changed = false;
        if (!target && other.target) {
            target = other.target;
            changed = true;
        }

        if (!symbolLength && other.symbolLength) {
            symbolLength = other.symbolLength;
            kind = other.kind;
            isDefinition = other.isDefinition;
            symbolName = other.symbolName;
            changed = true;
        }
        const int oldSize = references.size();
        if (!oldSize) {
            references = other.references;
            if (!references.isEmpty())
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
    uint16_t ref;
};

#endif
