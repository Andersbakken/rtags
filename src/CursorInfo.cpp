#include "CursorInfo.h"
#include "RTagsClang.h"

const char *jsKindNames[] = {
    "JSInvalid",
    "JSVariable",
    "JSWeakVariable",
    "JSFunction",
    "JSReference"
};

String CursorInfo::kindSpelling(uint16_t kind)
{
    if (kind >= JSInvalid)
        return jsKindNames[kind - JSInvalid];
    return RTags::eatString(clang_getCursorKindSpelling(static_cast<CXCursorKind>(kind)));
}

String CursorInfo::toString(unsigned cursorInfoFlags, unsigned keyFlags) const
{
    String ret = String::format<1024>("SymbolName: %s\n"
                                      "Kind: %s\n"
                                      "Type: %s\n"
                                      "SymbolLength: %u\n"
                                      "%s" // range
                                      "%s" // enumValue
                                      "%s", // definition
                                      symbolName.constData(),
                                      kindSpelling().constData(),
                                      RTags::eatString(clang_getTypeKindSpelling(type)).constData(),
                                      symbolLength,
                                      start != -1 && end != -1 ? String::format<32>("Range: %d-%d\n", start, end).constData() : "",
#if CINDEX_VERSION_MINOR > 1
                                      kind == CXCursor_EnumConstantDecl ? String::format<32>("Enum Value: %lld\n", enumValue).constData() :
#endif
                                      "",
                                      isDefinition() ? "Definition\n" : "");

    if (!targets.isEmpty() && !(cursorInfoFlags & IgnoreTargets)) {
        ret.append("Targets:\n");
        for (Set<Location>::const_iterator tit = targets.begin(); tit != targets.end(); ++tit) {
            const Location &l = *tit;
            ret.append(String::format<128>("    %s\n", l.key(keyFlags).constData()));
        }
    }

    if (!references.isEmpty() && !(cursorInfoFlags & IgnoreReferences)) {
        ret.append("References:\n");
        for (Set<Location>::const_iterator rit = references.begin(); rit != references.end(); ++rit) {
            const Location &l = *rit;
            ret.append(String::format<128>("    %s\n", l.key(keyFlags).constData()));
        }
    }
    return ret;
}

int CursorInfo::targetRank(const CursorInfo &target) const
{
    switch (target.kind) {
    case CXCursor_Constructor: // this one should be more than class/struct decl
        return 1;
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_ClassTemplate:
        return 0;
    case CXCursor_FieldDecl:
    case CXCursor_VarDecl:
        return 3;
    case CXCursor_MacroDefinition:
        return 4;
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
        const int r = targetRank(ci);
        if (r > bestRank || (r == bestRank && ci.isDefinition())) {
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
    SymbolMap ret;
    const SymbolMap cursors = virtuals(loc, map);
    for (SymbolMap::const_iterator c = cursors.begin(); c != cursors.end(); ++c) {
        for (Set<Location>::const_iterator it = c->second.references.begin(); it != c->second.references.end(); ++it) {
            const SymbolMap::const_iterator found = RTags::findCursorInfo(map, *it);
            if (found == map.end())
                continue;
            if (RTags::isReference(found->second.kind)) { // is this always right?
                ret[*it] = found->second;
            } else if (kind == CXCursor_Constructor && (found->second.kind == CXCursor_VarDecl || found->second.kind == CXCursor_FieldDecl)) {
                ret[*it] = found->second;
            }
        }
    }
    return ret;
}

enum Mode {
    ClassRefs,
    VirtualRefs,
    NormalRefs
};

static inline void allImpl(const SymbolMap &map, const Location &loc, const CursorInfo &info, SymbolMap &out, Mode mode, unsigned kind)
{
    if (out.contains(loc))
        return;
    out[loc] = info;
    typedef SymbolMap (CursorInfo::*Function)(const SymbolMap &map) const;
    const SymbolMap targets = info.targetInfos(map);
    for (SymbolMap::const_iterator t = targets.begin(); t != targets.end(); ++t) {
        bool ok = false;
        switch (mode) {
        case VirtualRefs:
        case NormalRefs:
            ok = (t->second.kind == kind);
            break;
        case ClassRefs:
            ok = (t->second.isClass()
                  || t->second.kind == CXCursor_Destructor
                  || t->second.kind == CXCursor_Constructor);
            break;
        }
        if (ok)
            allImpl(map, t->first, t->second, out, mode, kind);
    }
    const SymbolMap refs = info.referenceInfos(map);
    for (SymbolMap::const_iterator r = refs.begin(); r != refs.end(); ++r) {
        switch (mode) {
        case NormalRefs:
            out[r->first] = r->second;
            break;
        case VirtualRefs:
            if (r->second.kind == kind) {
                allImpl(map, r->first, r->second, out, mode, kind);
            } else {
                out[r->first] = r->second;
            }
            break;
        case ClassRefs:
            if (info.isClass()) // for class/struct we want the references inserted directly regardless and also recursed
                out[r->first] = r->second;
            if (r->second.isClass()
                || r->second.kind == CXCursor_Destructor
                || r->second.kind == CXCursor_Constructor) { // if is a constructor/destructor/class reference we want to recurse it
                allImpl(map, r->first, r->second, out, mode, kind);
            }
        }
    }
}

SymbolMap CursorInfo::allReferences(const Location &loc, const SymbolMap &map) const
{
    SymbolMap ret;
    Mode mode = NormalRefs;
    switch (kind) {
    case CXCursor_Constructor:
    case CXCursor_Destructor:
        mode = ClassRefs;
        break;
    case CXCursor_CXXMethod:
        mode = VirtualRefs;
        break;
    default:
        mode = isClass() ? ClassRefs : VirtualRefs;
        break;
    }

    allImpl(map, loc, *this, ret, mode, kind);
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

String CursorInfo::displayName() const
{
    // int paren = symbolName.indexOf('(');
    // int bracket = symbolName.indexOf('<');
    int end = symbolName.indexOf('(');
    if (end == -1) {
        end = symbolName.indexOf('<');
        if (end == -1)
            end = symbolName.size();
    }
    int start = end;
    while (start > 0 && RTags::isSymbol(symbolName.at(start - 1)))
        --start;
    return symbolName.mid(start, end - start);

    // int end = symbolName.indexOf('(');
    // int start = 0;
    // if (end != -1) {
    //     start = symbolName.lastIndexOf("::", paren);
    //     if (start == -1) {
    //         start = symbolName.lastIndexOf(' ');
    // }
}

bool CursorInfo::isValid(const Location &location) const
{
    const Path p = location.path();
    bool ret = false;
    FILE *f = fopen(p.constData(), "r");
    if (f && fseek(f, location.offset(), SEEK_SET) != -1) {
        const String display = displayName();
        // int paren = symbolName.indexOf('(');
        // int bracket = symbolName.indexOf('<');
        // int end = symbolName.size();
        // if (paren != -1) {
        //     if (bracket != -1) {
        //         end = std::min(paren, bracket);
        //     } else {
        //         end = paren;
        //     }
        // } else if (bracket != -1) {
        //     end = bracket;
        // }
        // int start = end;
        // while (start > 0 && RTags::isSymbol(symbolName.at(start - 1)))
        //     --start;

        char buf[1024];
        const int length = display.size();
        if (length && length < static_cast<int>(sizeof(buf)) - 1 && fread(buf, std::min<int>(length, sizeof(buf) - 1), 1, f)) {
            buf[length] = '\0';
            ret = display == buf;
            if (!ret) {
                error("Different:\n[%s]\n[%s]", buf, display.constData());
            }
        }
    }
    if (f)
        fclose(f);
    return ret;
}
