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

CursorInfo findCursorInfo(leveldb::DB *db, const RTags::Location &location)
{
    const leveldb::ReadOptions readopts;
    leveldb::Iterator* it = db->NewIterator(readopts);
    const QByteArray needle = location.key(RTags::Location::Padded);
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
        const RTags::Location loc = RTags::Location::fromKey(key);
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
    if (found && !cursorInfo.symbolLength)
        cursorInfo = Rdm::readValue<Rdm::CursorInfo>(it);
#if 0
    if (list.isEmpty()) {
        it->SeekToFirst();
        while (it->Valid()) {
            const leveldb::Slice k = it->key();
            const QByteArray key = QByteArray::fromRawData(k.data(), k.size());
            RTags::Location loc = RTags::Location::fromKey(key);
            debug() << key << loc;
            it->Next();
        }
    }
#endif
    delete it;
    if (!found)
        cursorInfo.clear();
    // error() << "found" << found << location << cursorInfo.target << cursorInfo.references << cursorInfo.symbolLength;
    return cursorInfo;
}
}

