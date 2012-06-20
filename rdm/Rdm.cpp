#include "Rdm.h"
#include "Database.h"
#include "CursorInfo.h"
#include "MemoryMonitor.h"

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
    const ByteArray name = eatString(clang_getCursorSpelling(cursor));
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

static quint64 sMaxMemoryUsage = 0;
bool waitForMemory(int maxMs)
{
    QElapsedTimer timer;
    timer.start();
    static quint64 last = 0;
    do {
        QElapsedTimer timer;
        timer.start();
        const quint64 mem = MemoryMonitor::usage();
        int elapsed = timer.elapsed();
        static int total = 0;
        total += elapsed;
        printf("We're at %lld, max is %lld (was at %lld) %d %d\n", mem, sMaxMemoryUsage, last, total, elapsed);
        if (mem < sMaxMemoryUsage) {
            return true;
        } else if (mem < last || !last) {
            sleep(1);
        } else {
            sleep(2); // yes!
        }
        last = mem;
    } while (maxMs <= 0 || timer.elapsed() >= maxMs);
    return false;
}

void setMaxMemoryUsage(quint64 max)
{
    sMaxMemoryUsage = max;
}

int writeSymbolNames(SymbolNameHash &symbolNames)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Write);

    Batch batch(db);
    int totalWritten = 0;

    SymbolNameHash::iterator it = symbolNames.begin();
    const SymbolNameHash::const_iterator end = symbolNames.end();
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

int writeDependencies(const DependencyHash &dependencies)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::Dependency, ScopedDB::Write);

    Batch batch(db);
    int totalWritten = 0;
    DependencyHash::const_iterator it = dependencies.begin();
    const DependencyHash::const_iterator end = dependencies.end();
    char buf[4];
    const Slice key(buf, 4);
    while (it != end) {
        memcpy(buf, &it->first, sizeof(buf));
        Set<quint32> added = it->second;
        Set<quint32> current = db->value<Set<quint32> >(key);
        const int oldSize = current.size();
        if (current.unite(added).size() > oldSize) {
            totalWritten += batch.add(key, current);
        }
        ++it;
    }
    return totalWritten;
}
int writePchDepencies(const Hash<Path, Set<quint32> > &pchDependencies)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::General, ScopedDB::Write);
    if (!pchDependencies.isEmpty())
        return db->setValue("pchDependencies", pchDependencies);
    return 0;
}
int writeFileInformation(quint32 fileId, const QList<ByteArray> &args, time_t lastTouched)
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

int writePchUSRHashes(const Hash<Path, PchUSRHash> &pchUSRHashes)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::PCHUsrHashes, ScopedDB::Write);
    int totalWritten = 0;
    Batch batch(db);
    for (Hash<Path, PchUSRHash>::const_iterator it = pchUSRHashes.begin(); it != pchUSRHashes.end(); ++it) {
        totalWritten += batch.add(it->first, it->second);
    }
    return totalWritten;
}

int writeSymbols(SymbolHash &symbols, const ReferenceHash &references, quint32 fileId)
{
    QElapsedTimer timer;
    timer.start();
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Write);
    Batch batch(db);
    int totalWritten = 0;

    if (!references.isEmpty()) {
        const ReferenceHash::const_iterator end = references.end();
        for (ReferenceHash::const_iterator it = references.begin(); it != end; ++it) {
            CursorInfo &ci = symbols[it->second.first];
            ci.references.insert(it->first);
            if (it->second.second != Rdm::NormalReference) {
                CursorInfo &other = symbols[it->first];
                // qDebug() << "trying to join" << it->first << "and" << it->second.first;
                if (other.target.isNull())
                    other.target = it->second.first;
                if (ci.target.isNull())
                    ci.target = it->first;
            }
        }
    }
    if (!symbols.isEmpty()) {
        SymbolHash::iterator it = symbols.begin();
        const SymbolHash::const_iterator end = symbols.end();
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

int dirty(const Set<quint32> &dirtyFileIds)
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
                    i = locations.erase(i);
                    ++ret;
                } else {
                    ++i;
                }
            }
            if (changed) {
                if (locations.isEmpty()) {
                    debug() << "No references to" << it->key() << "anymore. Removing";
                    db->remove(it->key());
                } else {
                    debug() << "References to" << it->key() << "modified. Changing";
                    db->setValue<Set<Location> >(it->key(), locations);
                }
            }
            it->next();
        }
    }
    error() << "dirtied" << dirtyFileIds.size() << "files in" << timer.elapsed() << "ms";
    return ret;
}

QList<ByteArray> compileArgs(quint32 fileId)
{
    ScopedDB db = Server::instance()->db(Server::FileInformation, ScopedDB::Read);
    const char *ch = reinterpret_cast<const char*>(&fileId);
    const Slice key(ch, sizeof(fileId));
    FileInformation fi = db->value<FileInformation>(key);
    return fi.compileArgs;
}

int EventObject::typeForName(const ByteArray &name)
{
    const QMetaObject m = staticMetaObject;
    for (int i = 0; i < m.enumeratorCount(); ++i) {
        const int idx = m.indexOfEnumerator("Type");
        if (idx >= 0) {
            const QMetaEnum en = m.enumerator(idx);
            if (name.contains('|'))
                return en.keysToValue(name.constData());
            else
                return en.keyToValue(name.constData());
        }
    }
    return -1;
}

}
