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

#include "Location.h"

#include "rct/Rct.h"
#include "RTags.h"
#include "Server.h"
#include "Project.h"
#include "ClangIndexer.h"

Hash<Path, uint32_t> Location::sPathsToIds;
Hash<uint32_t, Path> Location::sIdsToPaths;
uint32_t Location::sLastId = 0;
std::mutex Location::sMutex;
static inline uint64_t createMask(int startBit, int bitCount)
{
    uint64_t mask = 0;
    for (int i=startBit; i<startBit + bitCount; ++i) {
        mask |= (static_cast<uint64_t>(1) << i);
    }
    return mask;
}

const uint64_t Location::FILEID_MASK = createMask(0, FileBits);
const uint64_t Location::LINE_MASK = createMask(FileBits, LineBits);
const uint64_t Location::COLUMN_MASK = createMask(FileBits + LineBits, ColumnBits);

String Location::toString(Flags<ToStringFlag> flags, Hash<Path, String> *contextCache) const
{
    if (isNull())
        return String();
    const unsigned int l = line();
    const unsigned int c = column();
    int extra = RTags::digits(l) + RTags::digits(c) + 3;
    String ctx;
    if (flags & Location::ShowContext) {
        ctx += '\t';
        ctx += context(flags, contextCache);
        extra += ctx.size();
    }

    Path p = path();
    if (!(flags & AbsolutePath) && Server::instance()) {
        Server *server = Server::instance();
        if (std::shared_ptr<Project> pp = server->currentProject()) {
            const Path projectPath = pp->path();
            if (!projectPath.isEmpty() && p.startsWith(projectPath))
                p.remove(0, projectPath.size());
        }
    }

    String ret(p.size() + extra, ' ');

    const int w = snprintf(ret.data(), ret.size() + extra + 1, "%s:%d:%d:", p.constData(), l, c);
    if (!ctx.isEmpty()) {
        memcpy(ret.data() + w, ctx.constData(), ctx.size());
    }
    return ret;
}

String Location::context(Flags<ToStringFlag> flags, Hash<Path, String> *cache) const
{
    String copy;
    String *code = 0;
    const Path p = path();
    if (cache) {
        String &ref = (*cache)[p];
        if (ref.isEmpty()) {
            ref = p.readAll();
        }
        code = &ref;
    } else {
        copy = p.readAll();
        code = &copy;
    }

    String ret;
    if (!code->isEmpty()) {
        unsigned int l = line();
        if (!l)
            return String();
        const char *ch = code->constData();
        while (--l) {
            ch = strchr(ch, '\n');
            if (!ch)
                return String();
            ++ch;
        }
        const char *end = strchr(ch, '\n');
        if (!end)
            return String();

        ret.assign(ch, end - ch);
        // error() << "foobar" << ret << bool(flags & NoColor);
        if (!(flags & NoColor)) {
            const size_t col = column() - 1;
            if (col + 1 < ret.size()) {
                size_t last = col;
                if (ret.at(last) == '~')
                    ++last;
                while (ret.size() > last && (isalnum(ret.at(last)) || ret.at(last) == '_'))
                    ++last;
                static const char *color = "\x1b[32;1m"; // dark yellow
                static const char *resetColor = "\x1b[0;0m";
                // error() << "foobar"<< end << col << ret.size();
                ret.insert(last, resetColor);
                ret.insert(col, color);
            }
            // printf("[%s]\n", ret.constData());
        }
    }
    return ret;
}

const Path &Location::sandboxRoot()
{
    if (Server::instance()) {
        return Server::instance()->options().sandboxRoot;
    } else {
        return ClangIndexer::serverSandboxRoot();
    }
}

const char *RELSBROOT = "[[SBROOT]]";
static inline bool pathStartWithRELSBROOT(const Path &path)
{
    return (strncmp(path.c_str(), RELSBROOT, strlen(RELSBROOT)) == 0);
}

bool Location::containRelativePath(const String & str)
{
    return (str.indexOf(RELSBROOT) != std::string::npos);
}

void Location::strPathToSbRoot(Path &path)
{
    auto idx = path.indexOf(RELSBROOT);
    if (idx != std::string::npos) {
        path.replace(idx, strlen(RELSBROOT), Location::sandboxRoot());
    }
}

bool Location::containSandboxRoot(const String & str)
{
    if (Location::sandboxRoot().isEmpty()) return false;
    return (str.indexOf(Location::sandboxRoot()) != std::string::npos);
}

String Location::replaceRelativeWithFullPath(const String & key)
{
    if (!Location::sandboxRoot().isEmpty()) {
        auto idx = key.indexOf(RELSBROOT);
        if (idx != std::string::npos) {
            String keyCpy = key;
            keyCpy.replace(idx, strlen(RELSBROOT), Location::sandboxRoot());
            return keyCpy;
        }
    }
    return key;
}

String Location::replaceFullWithRelativePath(const String & key)
{
    if (!Location::sandboxRoot().isEmpty()) {
        auto idx = key.indexOf(Location::sandboxRoot());
        if (idx != std::string::npos) {
            String keyCpy = key;
            keyCpy.replace(idx, Location::sandboxRoot().size(), RELSBROOT);
            return keyCpy;
        }
    }
    return key;
}

void Location::convertPathRelative(Path & path)
{
    if (!path.isEmpty() && !pathStartWithRELSBROOT(path)) {
        // assert(path.isAbsolute());
        const Path &root = Location::sandboxRoot();
        if (!root.isEmpty() && path.startsWith(root)) {
            assert(root.endsWith('/'));
            path.replace(0, root.size(), RELSBROOT);
        }
    }
}

void Location::convertPathFull(Path &path)
{
    if (pathStartWithRELSBROOT(path)) {
        const Path &root = Location::sandboxRoot();
        if (!root.isEmpty()) {
            assert(root.endsWith('/'));
            path.replace(0, strlen(RELSBROOT), root.c_str());
        }
    }
}

void Location::saveFileIds()
{
    assert(Server::instance());
    Server::instance()->saveFileIds();
}
