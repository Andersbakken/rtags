#ifndef CursorInfo_h
#define CursorInfo_h

#include "ByteArray.h"
#include "Location.h"
#include "Path.h"
#include <clang-c/Index.h>

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

    bool dirty(const Set<uint32_t> &dirty)
    {
        bool changed = false;
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
        return changed;
    }

    bool isEmpty() const
    {
        assert((symbolLength || symbolName.isEmpty()) && (symbolLength || kind == CXCursor_FirstInvalid)); // these should be coupled
        return !symbolLength && !target && references.isEmpty();
    }

    bool unite(const CursorInfo &other)
    {
        bool changed = false;
        if (!target && other.target.isValid()) {
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
};

#endif
