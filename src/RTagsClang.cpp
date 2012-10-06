#include "RTagsClang.h"

namespace RTags {
ByteArray eatString(CXString str)
{
    const ByteArray ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

ByteArray cursorToString(CXCursor cursor, unsigned flags)
{
    const CXCursorKind kind = clang_getCursorKind(cursor);
    ByteArray ret;
    ret.reserve(256);
    ret += eatString(clang_getCursorKindSpelling(kind));

    switch (RTags::cursorType(kind)) {
    case Reference:
        ret += " r";
        break;
    case Cursor:
        ret += " c";
        break;
    case Other:
        ret += " o";
        break;
    case Include:
        ret += " i";
        break;
    }

    const ByteArray name = eatString(clang_getCursorDisplayName(cursor));
    const ByteArray other = eatString(clang_getCursorSpelling(cursor));
    if (!name.isEmpty())
        ret += " " + name;
    if (other != name && !other.isEmpty())
        ret += " " + other;

    if (clang_isCursorDefinition(cursor))
        ret += " def";

    if (flags & IncludeUSR)
        ret += " " + eatString(clang_getCursorUSR(cursor));

    CXFile file;
    unsigned off, line, col; //presumedLine, presumedCol, instantiationLoc, expansionLoc;
    CXSourceLocation location = clang_getCursorLocation(cursor);
    clang_getSpellingLocation(location, &file, &line, &col, &off);
    // clang_getPresumedLocation(location, 0, &presumedLine, &presumedCol);
    // clang_getInstantiationLocation(location, 0, 0, 0, &instantiationLoc);
    // clang_getExpansionLocation(location, 0, 0, 0, &expansionLoc);

    const Str fileName(clang_getFileName(file));
    if (fileName.data() && *fileName.data()) {
        ret += ' ';
        ret += fileName.data();
        ret += ',';
        ret += ByteArray::number(off);

        if (flags & IncludeRange) {
            ret += " (";
            CXSourceRange range = clang_getCursorExtent(cursor);
            unsigned start, end;
            clang_getSpellingLocation(clang_getRangeStart(range), 0, 0, 0, &start);
            clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
            ret += ByteArray::number(start);
            ret += '-';
            ret += ByteArray::number(end);
            ret += ')';
        }

        // if (presumedLine != line || presumedCol != col)
        //     ret += ByteArray::snprintf<32>("presumed: %d:%d", presumedLine, presumedCol);
        // if (instantiationLoc != off)
        //     ret += ByteArray::snprintf<32>("instantiation: %d", instantiationLoc);
        // if (expansionLoc != off)
        //     ret += ByteArray::snprintf<32>("expansion: %d", expansionLoc);

    }
    return ret;
}


SymbolMap::const_iterator findCursorInfo(const SymbolMap &map, const Location &location)
{
    if (map.isEmpty())
        return map.end();
    SymbolMap::const_iterator it = map.lower_bound(location);
    if (it == map.end()) {
        --it;
    } else {
        const int cmp = it->first.compare(location);
        if (!cmp)
            return it;
        --it;
    }
    if (location.fileId() != it->first.fileId())
        return map.end();
    const int off = location.offset() - it->first.offset();
    if (it->second.symbolLength > off)
        return it;
    return map.end();
}

}
