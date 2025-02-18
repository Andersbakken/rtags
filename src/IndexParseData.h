/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef IndexParseData_h
#define IndexParseData_h

#include <cstdint>
#include <functional>
#include <algorithm>
#include <unordered_map>
#include <utility>

#include "rct/Path.h"
#include "rct/List.h"
#include "rct/Serializer.h"
#include "rct/Log.h"
#include "RTags.h"
#include "rct/Connection.h"
#include "Match.h"
#include "Location.h"
#include "Sandbox.h"
#include "Source.h"
#include "rct/Hash.h"
#include "rct/String.h"

class IndexParseData
{
public:
    Path project;

    void clearSources()
    {
        lastModifiedMs = 0;
        sources.clear();
    }

    void clear()
    {
        compileCommandsFileId = 0;
        lastModifiedMs = 0;
        sources.clear();
        environment.clear();
    }

    uint32_t compileCommandsFileId { 0 };
    uint64_t lastModifiedMs { 0 };
    Sources sources;
    List<String> environment;

    bool empty() const { return environment.empty() && sources.empty(); }
    bool write(const std::function<bool(const String &)> &write, const Match &match = Match()) const;
};

inline Serializer &operator<<(Serializer &s, const IndexParseData &data)
{
    s << Sandbox::encoded(data.project);
    if (!data.compileCommandsFileId) {
        s << Path();
    } else {
        s << Location::path(data.compileCommandsFileId);
    }
    s << data.compileCommandsFileId << data.lastModifiedMs << data.sources << Sandbox::encoded(data.environment);
    return s;
}

inline Deserializer &operator>>(Deserializer &s, IndexParseData &data)
{
    s >> data.project;
    data.clear();
    Path file;
    s >> file;
    s >> data.compileCommandsFileId;
    if (data.compileCommandsFileId) {
        Location::set(file, data.compileCommandsFileId);
    }
    s >> data.lastModifiedMs;
    s >> data.sources >> data.environment;
    Sandbox::decode(data.environment);
    return s;
}

#endif
