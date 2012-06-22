#include "Rdm.h"
#include "Database.h"
#include "CursorInfo.h"
#include "ScopedDB.h"
#include "Server.h"
#include "MemoryMonitor.h"
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
        ret += " " + fileName + ':' + ByteArray::number(line) + ":" + ByteArray::number(col) + ": (" + ByteArray::number(off) + ")"; // + eatString(clang_getCursorUSR(cursor));
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
        if (!found)
            it->previous();
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
                debug("offsets wrong symbolLength %d offset %d %d/%d", cursorInfo.symbolLength,
                      off, location.offset(), loc.offset());
            }
        } else {
            debug() << "wrong path" << location.path() << loc.path() << key;
        }
    }
    if (found) {
        if (!cursorInfo.symbolLength) {
            cursorInfo = it->value<CursorInfo>();
        }
        if (loc) {
            *loc = Location::fromKey(it->key().data());
        }
    }
    if (!found) {
        // printf("[%s] %s:%d: if (!found) {\n", __func__, __FILE__, __LINE__);
        cursorInfo.clear();
    }
    // error() << "found" << found << location << cursorInfo.target << cursorInfo.references << cursorInfo.symbolLength
    //         << cursorInfo.symbolName;
    return cursorInfo;
}

int writeSymbolNames(SymbolNameMap &symbolNames)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Write);

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
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::Dependency, ScopedDB::Write);

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
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::General, ScopedDB::Write);
    if (!pchDependencies.isEmpty())
        return db->setValue("pchDependencies", pchDependencies);
    return 0;
}
int writeFileInformation(uint32_t fileId, const List<ByteArray> &args, time_t lastTouched)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::FileInformation, ScopedDB::Write);
    if (Location::path(fileId).isHeader() && !isPch(args)) {
        error() << "Somehow we're writing fileInformation for a header that isn't pch"
                << Location::path(fileId) << args << lastTouched;
    }
    const char *ch = reinterpret_cast<const char*>(&fileId);
    return db->setValue(Slice(ch, sizeof(fileId)), FileInformation(lastTouched, args));
}

int writePchUSRMapes(const Map<Path, PchUSRMap> &pchUSRMapes)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::PCHUsrMapes, ScopedDB::Write);
    int totalWritten = 0;
    Batch batch(db);
    for (Map<Path, PchUSRMap>::const_iterator it = pchUSRMapes.begin(); it != pchUSRMapes.end(); ++it) {
        totalWritten += batch.add(it->first, it->second);
    }
    return totalWritten;
}

int writeSymbols(SymbolMap &symbols, const ReferenceMap &references, uint32_t fileId)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Write);
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
            const CursorInfo added = it->second;
            if (it->first.fileId() != fileId) {
                bool ok;
                CursorInfo current = db->value<CursorInfo>(key, &ok);
                if (ok) {
                    if (current.unite(added))
                        totalWritten += batch.add(key, current);
                    ++it;
                    continue;
                }
            }
            totalWritten += batch.add(key, added);
            ++it;
        }
    }
    return totalWritten;
}

int dirty(const Set<uint32_t> &dirtyFileIds)
{
    QElapsedTimer timer;
    timer.start();
    // ### we should probably have a thread or something that stats each file we have in the db and calls dirty if the file is gone
    int ret = 0;
    debug() << "dirty" << dirtyFileIds;
    {
        ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Write);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            const Slice key = it->key();
            Q_ASSERT(key.size() == 8);
            const Location loc = Location::fromKey(key.data());
            // debug() << "looking at" << key;
            if (dirtyFileIds.contains(loc.fileId())) {
                db->remove(key);
            } else {
                CursorInfo cursorInfo = it->value<CursorInfo>();
                if (cursorInfo.dirty(dirtyFileIds)) {
                    // ### should we remove the whole cursorInfo if its target and all the references are gone?
                    // if (cursorInfo.target.isNull() && cursorInfo.references.isEmpty()) {
                    //     debug() << "CursorInfo is empty now. removing" << key;
                    //     db->remove(key);
                    // } else {
                    db->setValue<CursorInfo>(key, cursorInfo);
                    ++ret;
                    // }
                }
            }
            it->next();
        }
    }

    {
        ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Write);

        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            Set<Location> locations = it->value<Set<Location> >();
            Set<Location>::iterator i = locations.begin();
            bool changed = false;
            while (i != locations.end()) {
                if (dirtyFileIds.contains((*i).fileId())) {
                    changed = true;
                    locations.erase(i++);
                    ++ret;
                } else {
                    ++i;
                }
            }
            if (changed) {
                if (locations.isEmpty()) {
                    debug() << "No references to " << it->key() << " anymore. Removing";
                    db->remove(it->key());
                } else {
                    debug() << "References to " << it->key() << " modified. Changing";
                    db->setValue<Set<Location> >(it->key(), locations);
                }
            }
            it->next();
        }
    }
    warning() << "dirtied " << dirtyFileIds.size() << " files in " << timer.elapsed() << "ms";
    return ret;
}

List<ByteArray> compileArgs(uint32_t fileId)
{
    ScopedDB db = Server::instance()->db(Server::FileInformation, ScopedDB::Read);
    const char *ch = reinterpret_cast<const char*>(&fileId);
    const Slice key(ch, sizeof(fileId));
    FileInformation fi = db->value<FileInformation>(key);
    return fi.compileArgs;
}

}
