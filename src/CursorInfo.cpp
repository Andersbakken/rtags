#include "CursorInfo.h"
#include "RTags.h"

ByteArray CursorInfo::toString() const
{
    ByteArray ret(16384, '\0');
    char *buf = ret.data();
    int pos = snprintf(buf, ret.size(), "CursorInfo(symbolLength: %u symbolName: %s kind: %s%s",
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

static inline int cursorRank(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_VarDecl:
    case CXCursor_FieldDecl:
        return 2;
    case CXCursor_Constructor: // this one should be more than class/struct decl
        return 1;
    default:
        return 0;
    }
}

CursorInfo CursorInfo::bestTarget(const SymbolMap &map, Location *loc) const
{
    const Map<Location, CursorInfo> targets = targetInfos(map);

    Map<Location, CursorInfo>::const_iterator best = targets.end();
    int bestRank = -1;
    for (Map<Location, CursorInfo>::const_iterator it = targets.begin(); it != targets.end(); ++it) {
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

Map<Location, CursorInfo> CursorInfo::targetInfos(const SymbolMap &map) const
{
    Map<Location, CursorInfo> ret;
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

Map<Location, CursorInfo> CursorInfo::referenceInfos(const SymbolMap &map) const
{
    Map<Location, CursorInfo> ret;
    for (Set<Location>::const_iterator it = references.begin(); it != references.end(); ++it) {
        SymbolMap::const_iterator found = RTags::findCursorInfo(map, *it);
        if (found != map.end()) {
            ret[*it] = found->second;
        }
    }
    return ret;
}
