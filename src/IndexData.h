#ifndef IndexData_h
#define IndexData_h

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

#include <cstdint>
#include <rct/Set.h>
#include "Source.h"
#include <rct/String.h>
#include "RTags.h"

class IndexData
{
public:
    IndexData(uint32_t f)
        : parseTime(0), key(0), id(0), flags(f)
    {}

    Set<uint32_t> visitedFiles() const
    {
        Set<uint32_t> ret;
        for (Hash<uint32_t, bool>::const_iterator it = visited.begin(); it != visited.end(); ++it) {
            if (it->second)
                ret.insert(it->first);
        }
        return ret;
    }

    Set<uint32_t> blockedFiles() const
    {
        Set<uint32_t> ret;
        for (Hash<uint32_t, bool>::const_iterator it = visited.begin(); it != visited.end(); ++it) {
            if (!it->second)
                ret.insert(it->first);
        }
        return ret;
    }

    uint32_t fileId() const
    {
        uint32_t fileId, buildRootId;
        Source::decodeKey(key, fileId, buildRootId);
        return fileId;
    }

    uint64_t parseTime, key;
    SymbolMap symbols;
    SymbolNameMap symbolNames;
    DependencyMap dependencies;
    UsrMap usrMap, pendingReferenceMap;
    String message; // used as output for dump when flags & Dump
    FixItMap fixIts;
    String xmlDiagnostics;
    Hash<uint32_t, bool> visited;
    uint64_t id;
    const uint32_t flags; // indexerjobflags
};

#endif
