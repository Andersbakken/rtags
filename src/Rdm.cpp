#include "Rdm.h"
#include "Database.h"
#include "CursorInfo.h"
#include "ScopedDB.h"
#include "Server.h"
#include "MemoryMonitor.h"
#include "Timer.h"
#include <List.h>

namespace Rdm {
ByteArray eatString(CXString str)
{
    const ByteArray ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

ByteArray cursorToString(CXCursor cursor)
{
    ByteArray ret = eatString(clang_getCursorKindSpelling(clang_getCursorKind(cursor)));
    const ByteArray name = eatString(clang_getCursorDisplayName(cursor));
    if (!name.isEmpty())
        ret += " " + name;

    CXFile file;
    unsigned off, line, col;
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    clang_getSpellingLocation(loc, &file, &line, &col, &off);
    const ByteArray fileName = eatString(clang_getFileName(file));
    if (!fileName.isEmpty()) {
        ret += " " + fileName + ',' + ByteArray::number(off);
    }
    return ret;
}

CursorInfo findCursorInfo(Database *db, const Location &location, Location *loc)
{
    RTags::Ptr<Iterator> it(db->createIterator());
    char needleBuf[8];
    location.toKey(needleBuf);
    const Slice needle(needleBuf, 8);
    it->seek(needle);
    bool found = false;
    CursorInfo cursorInfo;
    if (it->isValid()) {
        const Slice key = it->key();
        found = (key == needle);
        if (!found) {
            it->previous();
        } else {
            cursorInfo = it->value<CursorInfo>();
        }
    } else {
        it->seekToLast();
    }

    if (!found && it->isValid()) {
        const Slice key = it->key();
        const Location loc = Location::fromKey(key.data());
        if (location.fileId() == loc.fileId()) {
            const int off = location.offset() - loc.offset();
            cursorInfo = it->value<CursorInfo>();
            if (cursorInfo.symbolLength > off) {
                found = true;
            } else {
                cursorInfo.clear();
                debug("offsets wrong symbolLength %d offset %d %d/%d", cursorInfo.symbolLength,
                      off, location.offset(), loc.offset());
            }
        } else {
            debug() << "wrong path" << location.path() << loc.path() << key;
        }
    }
    // assert(found == (cursorInfo.symbolLength != 0));
    if (found) {
        if (loc) {
            *loc = Location::fromKey(it->key().data());
        }
    }
    // error() << "found" << found << location << cursorInfo.target << cursorInfo.references << cursorInfo.symbolLength
    //         << cursorInfo.symbolName;
    return cursorInfo;
}

int writeSymbolNames(SymbolNameMap &symbolNames)
{
    Timer timer;
    ScopedDB db = Server::instance()->db(Server::SymbolName, ReadWriteLock::Write);

    Batch batch(db);
    int totalWritten = 0;

    SymbolNameMap::iterator it = symbolNames.begin();
    const SymbolNameMap::const_iterator end = symbolNames.end();
    while (it != end) {
        const char *key = it->first.constData();
        const Set<Location> added = it->second;
        bool ok;
        Set<Location> current = db->value<Set<Location> >(key, &ok);
        if (!ok) {
            totalWritten += batch.add(key, added);
        } else if (addTo(current, added)) {
            totalWritten += batch.add(key, current);
        }
        ++it;
    }

    return totalWritten;
}

int writeDependencies(const DependencyMap &dependencies)
{
    Timer timer;
    ScopedDB db = Server::instance()->db(Server::Dependency, ReadWriteLock::Write);

    Batch batch(db);
    int totalWritten = 0;
    DependencyMap::const_iterator it = dependencies.begin();
    const DependencyMap::const_iterator end = dependencies.end();
    char buf[4];
    const Slice key(buf, 4);
    while (it != end) {
        memcpy(buf, &it->first, sizeof(buf));
        Set<uint32_t> added = it->second;
        Set<uint32_t> current = db->value<Set<uint32_t> >(key);
        const int oldSize = current.size();
        if (current.unite(added).size() > oldSize) {
            totalWritten += batch.add(key, current);
        }
        ++it;
    }
    return totalWritten;
}
int writePchDepencies(const Map<Path, Set<uint32_t> > &pchDependencies)
{
    Timer timer;
    ScopedDB db = Server::instance()->db(Server::General, ReadWriteLock::Write);
    if (!pchDependencies.isEmpty())
        return db->setValue("pchDependencies", pchDependencies);
    return 0;
}
int writeFileInformation(uint32_t fileId, const List<ByteArray> &args, time_t lastTouched)
{
    Timer timer;
    ScopedDB db = Server::instance()->db(Server::FileInformation, ReadWriteLock::Write);
    if (Location::path(fileId).isHeader() && !isPch(args)) {
        error() << "Somehow we're writing fileInformation for a header that isn't pch"
                << Location::path(fileId) << args << lastTouched;
    }
    const char *ch = reinterpret_cast<const char*>(&fileId);
    return db->setValue(Slice(ch, sizeof(fileId)), FileInformation(lastTouched, args));
}

int writePchUSRMaps(const Map<Path, PchUSRMap> &pchUSRMaps)
{
    Timer timer;
    ScopedDB db = Server::instance()->db(Server::PCHUsrMaps, ReadWriteLock::Write);
    int totalWritten = 0;
    Batch batch(db);
    for (Map<Path, PchUSRMap>::const_iterator it = pchUSRMaps.begin(); it != pchUSRMaps.end(); ++it) {
        totalWritten += batch.add(it->first, it->second);
    }
    return totalWritten;
}

int writeSymbols(SymbolMap &symbols, const ReferenceMap &references)
{
    Timer timer;
    ScopedDB db = Server::instance()->db(Server::Symbol, ReadWriteLock::Write);
    Batch batch(db);
    int totalWritten = 0;

    if (!references.isEmpty()) {
        const ReferenceMap::const_iterator end = references.end();
        for (ReferenceMap::const_iterator it = references.begin(); it != end; ++it) {
            CursorInfo &ci = symbols[it->second.first];
            ci.references.insert(it->first);
            if (it->second.second != Rdm::NormalReference) {
                CursorInfo &other = symbols[it->first];
                // error() << "trying to join" << it->first << "and" << it->second.front();
                if (other.target.isNull())
                    other.target = it->second.first;
                if (ci.target.isNull())
                    ci.target = it->first;
            }
        }
    }
    if (!symbols.isEmpty()) {
        SymbolMap::iterator it = symbols.begin();
        const SymbolMap::const_iterator end = symbols.end();
        while (it != end) {
            char buf[8];
            it->first.toKey(buf);
            const Slice key(buf, 8);
            CursorInfo added = it->second;
            bool ok;
            CursorInfo current = db->value<CursorInfo>(key, &ok);
            if (!ok) {
                totalWritten += batch.add(key, added);
            } else if (current.unite(added)) {
                totalWritten += batch.add(key, current);
            }
            ++it;
        }
    }
    return totalWritten;
}

List<ByteArray> compileArgs(uint32_t fileId)
{
    ScopedDB db = Server::instance()->db(Server::FileInformation, ReadWriteLock::Read);
    const char *ch = reinterpret_cast<const char*>(&fileId);
    const Slice key(ch, sizeof(fileId));
    FileInformation fi = db->value<FileInformation>(key);
    return fi.compileArgs;
}
}
