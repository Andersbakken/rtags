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
        additionalReferences.clear();
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

        Set<Location> *sets[2] = { &references, &additionalReferences };
        for (int i=0; i<2; ++i) {
            Set<Location> &set = *sets[i];
            Set<Location>::iterator it = set.begin();
            while (it != set.end()) {
                if (dirty.contains(it->fileId())) {
                    changed = true;
                    set.erase(it++);
                } else {
                    ++it;
                }
            }
        }
        return changed;
    }

    bool isValid() const
    {
        return !isEmpty();
    }

    bool isEmpty() const
    {
        assert((symbolLength || symbolName.isEmpty()) && (symbolLength || kind == CXCursor_FirstInvalid)); // these should be coupled
        return !symbolLength && !target && references.isEmpty() && additionalReferences.isEmpty();
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
        const Set<Location> *srcs[2] = { &other.references, &other.additionalReferences };
        Set<Location> *targets[2] = { &references, &additionalReferences };
        for (int i=0; i<2; ++i) {
            const Set<Location> &src = *srcs[i];
            Set<Location> &target = *targets[i];
            const int oldSize = target.size();
            if (!oldSize) {
                target = src;
                if (!src.isEmpty())
                    changed = true;
            } else {
                target.unite(src);
                if (oldSize != target.size())
                    changed = true;
            }
        }

        return changed;
    }

    unsigned char symbolLength; // this is just the symbol name length e.g. foo => 3
    ByteArray symbolName; // this is fully qualified Foobar::Barfoo::foo
    CXCursorKind kind;
    bool isDefinition;
    Location target;
    Set<Location> references, additionalReferences;
};

#endif
