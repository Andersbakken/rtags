#include "Rdm.h"

namespace Rdm {
QByteArray eatString(CXString str)
{
    const QByteArray ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

QByteArray cursorToString(CXCursor cursor)
{
    QByteArray ret = eatString(clang_getCursorKindSpelling(clang_getCursorKind(cursor)));
    const QByteArray name = eatString(clang_getCursorSpelling(cursor));
    if (!name.isEmpty())
        ret += " " + name;

    CXFile file;
    unsigned off;
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    clang_getSpellingLocation(loc, &file, 0, 0, &off);
    const QByteArray fileName = eatString(clang_getFileName(file));
    if (!fileName.isEmpty()) {
        ret += " " + fileName + ',' + QByteArray::number(off);
    }
    return ret;
}

static QList<Path> sSystemPaths;
void initSystemPaths(const QList<Path> &paths)
{
    sSystemPaths = paths;
    qSort(sSystemPaths);
}

bool isSystem(const Path &path)
{
    if (!strncmp("/usr/", path.constData(), 5)) {
#ifdef Q_OS_BSD4
        if (!strncmp("home/", path.constData() + 5, 5))
            return false;
#endif
        return true;
    }
    return startsWith(sSystemPaths, path);
}

CursorInfo findCursorInfo(leveldb::DB *db, const Location &location, Location *loc)
{
    const leveldb::ReadOptions readopts;
    leveldb::Iterator* it = db->NewIterator(readopts);
    const QByteArray needle = location.key(Location::Padded);
    it->Seek(needle.constData());
    QList<QByteArray> list;
    bool found = false;
    Rdm::CursorInfo cursorInfo;
    if (it->Valid()) {
        const leveldb::Slice k = it->key();
        const QByteArray key = QByteArray::fromRawData(k.data(), k.size());
        found = (key == needle);
        if (!found)
            it->Prev();
    } else {
        it->SeekToLast();
    }
    if (!found && it->Valid()) {
        const leveldb::Slice k = it->key();
        const QByteArray key = QByteArray::fromRawData(k.data(), k.size());
        debug() << "key" << key << "needle" << needle;
        const Location loc = Location::fromKey(key);
        if (location.path == loc.path) {
            const int off = location.offset - loc.offset;
            cursorInfo = Rdm::readValue<Rdm::CursorInfo>(it);
            if (cursorInfo.symbolLength > off) {
                found = true;
            } else {
                debug("offsets wrong symbolLength %d offset %d %d/%d", cursorInfo.symbolLength,
                      off, location.offset, loc.offset);
            }
        } else {
            debug() << "wrong path" << location.path << loc.path << key;
        }
    }
    if (found) {
        if (!cursorInfo.symbolLength) {
            cursorInfo = Rdm::readValue<Rdm::CursorInfo>(it);
        }
        if (loc) {
            *loc = Location::fromKey(QByteArray::fromRawData(it->key().data(), it->key().size()));
        }
    }
    delete it;
    if (!found) {
        // printf("[%s] %s:%d: if (!found) {\n", __func__, __FILE__, __LINE__);
        cursorInfo.clear();
    }
    // error() << "found" << found << location << cursorInfo.target << cursorInfo.references << cursorInfo.symbolLength
    //         << cursorInfo.symbolName;
    return cursorInfo;
}
}

