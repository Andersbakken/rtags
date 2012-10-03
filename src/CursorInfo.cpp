#include "CursorInfo.h"
#include "RTagsClang.h"

ByteArray CursorInfo::toString(unsigned keyFlags) const
{
    ByteArray ret(16384, '\0');
    char *buf = ret.data();
    int pos = snprintf(buf, ret.size(), "CursorInfo(%ssymbolLength: %u symbolName: %s kind: %s%s",
                       start != -1 && end != -1 ? ByteArray::snprintf<16>("%d-%d ", start, end).constData() : "",
                       symbolLength, symbolName.constData(), RTags::eatString(clang_getCursorKindSpelling(kind)).constData(),
                       isDefinition ? " definition" : "");
    buf += pos;

    if (pos < ret.size() && !targets.isEmpty()) {
        int w = snprintf(buf, ret.size() - pos, " targets:\n");
        pos += w;
        buf += w;
        for (Set<Location>::const_iterator tit = targets.begin(); tit != targets.end() && w < ret.size(); ++tit) {
            const Location &l = *tit;
            w = snprintf(buf, ret.size() - pos, "    %s\n", l.key(keyFlags).constData());
            buf += w;
            pos += w;
        }
    }

    if (pos < ret.size() && !references.isEmpty()) {
        int w = snprintf(buf, ret.size() - pos, " references:\n");
        pos += w;
        buf += w;
        for (Set<Location>::const_iterator rit = references.begin(); rit != references.end() && w < ret.size(); ++rit) {
            const Location &l = *rit;
            w = snprintf(buf, ret.size() - pos, "    %s\n", l.key(keyFlags).constData());
            buf += w;
            pos += w;
        }
    }
    ret.truncate(pos);
    return ret;
}

int CursorInfo::cursorRank(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_Constructor: // this one should be more than class/struct decl
        return 1;
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_ClassTemplate:
        return 0;
    default:
        return 2;
    }
}

CursorInfo CursorInfo::bestTarget(const SymbolMap &map, Location *loc) const
{
    const SymbolMap targets = targetInfos(map);

    SymbolMap::const_iterator best = targets.end();
    int bestRank = -1;
    for (SymbolMap::const_iterator it = targets.begin(); it != targets.end(); ++it) {
        const CursorInfo &ci = it->second;
        const int r = cursorRank(ci.kind);
        if (r > bestRank) {
            bestRank = r;
            best = it;
        }
    }
    if (best != targets.end()) {
        if (loc)
            *loc = best->first;
        return best->second;
    }
    return CursorInfo();
}

SymbolMap CursorInfo::targetInfos(const SymbolMap &map) const
{
    SymbolMap ret;
    for (Set<Location>::const_iterator it = targets.begin(); it != targets.end(); ++it) {
        SymbolMap::const_iterator found = RTags::findCursorInfo(map, *it);
        if (found != map.end()) {
            ret[*it] = found->second;
        } else {
            ret[*it] = CursorInfo();
            // we need this one for inclusion directives which target a
            // non-existing CursorInfo
        }
    }
    return ret;
}

SymbolMap CursorInfo::referenceInfos(const SymbolMap &map) const
{
    SymbolMap ret;
    for (Set<Location>::const_iterator it = references.begin(); it != references.end(); ++it) {
        SymbolMap::const_iterator found = RTags::findCursorInfo(map, *it);
        if (found != map.end()) {
            ret[*it] = found->second;
        }
    }
    return ret;
}

SymbolMap CursorInfo::callers(const Location &loc, const SymbolMap &map) const
{
    assert(!RTags::isReference(kind));
    SymbolMap ret;
    const SymbolMap cursors = virtuals(loc, map);
    for (SymbolMap::const_iterator c = cursors.begin(); c != cursors.end(); ++c) {
        for (Set<Location>::const_iterator it = c->second.references.begin(); it != c->second.references.end(); ++it) {
            const SymbolMap::const_iterator found = RTags::findCursorInfo(map, *it);
            if (found == map.end())
                continue;
            if (RTags::isReference(found->second.kind)) { // is this always right?
                ret[*it] = found->second;
            }
        }
    }
    return ret;
}

static inline void allImpl(const SymbolMap &map, const Location &loc, const CursorInfo &info, SymbolMap &out, bool recurse)
{
    assert(!out.contains(loc));
    out[loc] = info;
    typedef SymbolMap (CursorInfo::*Function)(const SymbolMap &map) const;
    Function functions[] = { &CursorInfo::referenceInfos, &CursorInfo::targetInfos };
    for (unsigned i=0; i<sizeof(functions) / sizeof(Function); ++i) {
        const SymbolMap ret = (info.*functions[i])(map);
        for (SymbolMap::const_iterator r = ret.begin(); r != ret.end(); ++r) {
            if (!out.contains(r->first)) {
                if (recurse) {
                    allImpl(map, r->first, r->second, out, recurse);
                } else if (i == 0 || r->second.kind == info.kind) {
                    out[r->first] = r->second;
                }
            }
        }
    }
}


SymbolMap CursorInfo::allReferences(const Location &loc, const SymbolMap &map) const
{
    SymbolMap ret;
    bool recurse = false;
    switch (kind) {
    case CXCursor_Constructor:
    case CXCursor_Destructor:
        recurse = true;
        break;
    default:
        recurse = isClass();
        break;
    }

    allImpl(map, loc, *this, ret, recurse);
    return ret;
}

SymbolMap CursorInfo::virtuals(const Location &loc, const SymbolMap &map) const
{
    SymbolMap ret;
    ret[loc] = *this;
    const SymbolMap s = (kind == CXCursor_CXXMethod ? allReferences(loc, map) : targetInfos(map));
    for (SymbolMap::const_iterator it = s.begin(); it != s.end(); ++it) {
        if (it->second.kind == kind)
            ret[it->first] = it->second;
    }
    return ret;
}

SymbolMap CursorInfo::declarationAndDefinition(const Location &loc, const SymbolMap &map) const
{
    SymbolMap cursors;
    cursors[loc] = *this;

    Location l;
    CursorInfo t = bestTarget(map, &l);

    if (t.kind == kind)
        cursors[l] = t;
    return cursors;
}
