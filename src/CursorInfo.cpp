#include "CursorInfo.h"
#include "RTags.h"

ByteArray CursorInfo::toString() const
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
            w = snprintf(buf, ret.size() - pos, "    %s", l.key().constData());
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
            w = snprintf(buf, ret.size() - pos, "    %s", l.key().constData());
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

SymbolMap CursorInfo::callers(const SymbolMap &map) const
{
    assert(!RTags::isReference(kind));
    const CursorInfo other = bestTarget(map);
    const CursorInfo *infos[] = { this, (other.kind == kind ? &other : 0), 0 };
    SymbolMap ret;
    for (int i=0; infos[i]; ++i) {
        for (Set<Location>::const_iterator it = infos[i]->references.begin(); it != infos[i]->references.end(); ++it) {
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

SymbolMap CursorInfo::allReferences(const SymbolMap &map) const
{

}

SymbolMap CursorInfo::virtuals(const SymbolMap &map) const
{

}
