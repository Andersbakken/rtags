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
    struct CompileCommands {
        CompileCommands()
            : lastModifiedMs(0)
        {}
        CompileCommands(CompileCommands &&other)
            : lastModifiedMs(other.lastModifiedMs), sources(std::move(other.sources)), environment(std::move(other.environment))
        {
            other.lastModifiedMs = 0;
        }
        CompileCommands(const CompileCommands &other)
            : lastModifiedMs(other.lastModifiedMs), sources(other.sources), environment(other.environment)
        {}

        CompileCommands &operator=(CompileCommands &&other)
        {
            lastModifiedMs = other.lastModifiedMs;
            sources = std::move(other.sources);
            environment = std::move(other.environment);
            other.lastModifiedMs = 0;
            return *this;
        }

        CompileCommands &operator=(const CompileCommands &other)
        {
            lastModifiedMs = other.lastModifiedMs;
            sources = other.sources;
            environment = other.environment;
            return *this;
        }

        void clearSources()
        {
            lastModifiedMs = 0;
            sources.clear();
        }

        uint64_t lastModifiedMs;
        Sources sources;
        List<String> environment;
    };
    Hash<uint32_t, CompileCommands> compileCommands; // fileId for compile_commands.json -> CompileCommands
    List<String> environment;
    Sources sources;

    bool isEmpty() const { return compileCommands.isEmpty() && environment.isEmpty() && sources.isEmpty(); }
    bool write(const std::function<bool(const String &)> &write, const Match &match = Match()) const;
};

inline Serializer &operator<<(Serializer &s, const IndexParseData::CompileCommands &commands)
{
    s << commands.lastModifiedMs << commands.sources << Sandbox::encoded(commands.environment);
    return s;
}

inline Deserializer &operator>>(Deserializer &s, IndexParseData::CompileCommands &commands)
{
    s >> commands.lastModifiedMs >> commands.sources >> commands.environment;
    Sandbox::decode(commands.environment);
    return s;
}

inline Serializer &operator<<(Serializer &s, const IndexParseData &data)
{
    s << Sandbox::encoded(data.project) << static_cast<uint32_t>(data.compileCommands.size());
    for (const auto &pair : data.compileCommands) {
        s << Location::path(pair.first) << pair.first << pair.second;
    }
    s << data.sources << Sandbox::encoded(data.environment);
    return s;
}

inline Deserializer &operator>>(Deserializer &s, IndexParseData &data)
{
    s >> data.project;
    data.compileCommands.clear();
    uint32_t size;
    s >> size;
    while (size-- > 0) {
        Path file;
        s >> file;
        uint32_t fileId;
        s >> fileId;
        Location::set(file, fileId);
        s >> data.compileCommands[fileId];
    }
    s >> data.sources >> data.environment;
    Sandbox::decode(data.environment);
    return s;
}

#endif
