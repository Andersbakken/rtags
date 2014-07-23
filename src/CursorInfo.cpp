/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */


#include "CursorInfo.h"
#include "RTagsClang.h"

String CursorInfo::kindSpelling(uint16_t kind)
{
    return RTags::eatString(clang_getCursorKindSpelling(static_cast<CXCursorKind>(kind)));
}

String CursorInfo::toString(unsigned cursorInfoFlags, unsigned keyFlags) const
{
    String ret = String::format<1024>("SymbolName: %s\n"
                                      "Kind: %s\n"
                                      "Type: %s\n" // type
                                      "SymbolLength: %u\n"
                                      "%s" // range
                                      "%s" // enumValue
                                      "%s", // definition
                                      symbolName.constData(),
                                      kindSpelling().constData(),
                                      RTags::eatString(clang_getTypeKindSpelling(type)).constData(),
                                      symbolLength,
                                      startLine != -1 ? String::format<32>("Range: %d:%d-%d:%d\n", startLine, startColumn, endLine, endColumn).constData() : "",
#if CINDEX_VERSION_MINOR > 1
                                      kind == CXCursor_EnumConstantDecl ? String::format<32>("Enum Value: %lld\n", enumValue).constData() :
#endif
                                      "",
                                      isDefinition() ? "Definition\n" : "");

    if (!targets.isEmpty() && !(cursorInfoFlags & IgnoreTargets)) {
        ret.append("Targets:\n");
        for (auto tit = targets.begin(); tit != targets.end(); ++tit) {
            const Location &l = *tit;
            ret.append(String::format<128>("    %s\n", l.key(keyFlags).constData()));
        }
    }

    if (!references.isEmpty() && !(cursorInfoFlags & IgnoreReferences)) {
        ret.append("References:\n");
        for (auto rit = references.begin(); rit != references.end(); ++rit) {
            const Location &l = *rit;
            ret.append(String::format<128>("    %s\n", l.key(keyFlags).constData()));
        }
    }
    return ret;
}

int CursorInfo::targetRank(const std::shared_ptr<CursorInfo> &target) const
{
    switch (target->kind) {
    case CXCursor_Constructor: // this one should be more than class/struct decl
        return 1;
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_ClassTemplate:
        return 0;
    case CXCursor_FieldDecl:
    case CXCursor_VarDecl:
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
        // functiondecl and cxx method must be more than cxx
        // CXCursor_FunctionTemplate. Since constructors for templatatized
        // objects seem to come out as function templates
        return 3;
    case CXCursor_MacroDefinition:
        return 4;
    default:
        return 2;
    }
}

std::shared_ptr<CursorInfo> CursorInfo::bestTarget(const SymbolMap &map, Location *loc) const
{
    const SymbolMap targets = targetInfos(map);

    auto best = targets.end();
    int bestRank = -1;
    for (auto it = targets.begin(); it != targets.end(); ++it) {
        const std::shared_ptr<CursorInfo> &ci = it->second;
        const int r = targetRank(ci);
        if (r > bestRank || (r == bestRank && ci->isDefinition())) {
            bestRank = r;
            best = it;
        }
    }
    if (best != targets.end()) {
        if (loc)
            *loc = best->first;
        return best->second;
    }
    return std::shared_ptr<CursorInfo>();
}

SymbolMap CursorInfo::targetInfos(const SymbolMap &map) const
{
    SymbolMap ret;
    for (auto it = targets.begin(); it != targets.end(); ++it) {
        auto found = RTags::findCursorInfo(map, *it);
        if (found != map.end()) {
            ret[*it] = found->second;
        } else {
            ret[*it] = std::make_shared<CursorInfo>();
            // we need this one for inclusion directives which target a
            // non-existing CursorInfo
        }
    }
    return ret;
}

SymbolMap CursorInfo::referenceInfos(const SymbolMap &map) const
{
    SymbolMap ret;
    for (auto it = references.begin(); it != references.end(); ++it) {
        auto found = RTags::findCursorInfo(map, *it);
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
    for (auto c = cursors.begin(); c != cursors.end(); ++c) {
        for (auto it = c->second->references.begin(); it != c->second->references.end(); ++it) {
            const auto found = RTags::findCursorInfo(map, *it);
            if (found == map.end())
                continue;
            if (RTags::isReference(found->second->kind)) { // is this always right?
                ret[*it] = found->second;
            } else if (kind == CXCursor_Constructor && (found->second->kind == CXCursor_VarDecl || found->second->kind == CXCursor_FieldDecl)) {
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

static inline void allImpl(const SymbolMap &map, const Location &loc, const std::shared_ptr<CursorInfo> &info, SymbolMap &out, Mode mode, unsigned kind)
{
    if (out.contains(loc))
        return;
    out[loc] = info;
    const SymbolMap targets = info->targetInfos(map);
    for (auto t = targets.begin(); t != targets.end(); ++t) {
        bool ok = false;
        switch (mode) {
        case VirtualRefs:
        case NormalRefs:
            ok = (t->second->kind == kind);
            break;
        case ClassRefs:
            ok = (t->second->isClass() || t->second->kind == CXCursor_Destructor || t->second->kind == CXCursor_Constructor);
            break;
        }
        if (ok)
            allImpl(map, t->first, t->second, out, mode, kind);
    }
    const SymbolMap refs = info->referenceInfos(map);
    for (auto r = refs.begin(); r != refs.end(); ++r) {
        switch (mode) {
        case NormalRefs:
            out[r->first] = r->second;
            break;
        case VirtualRefs:
            if (r->second->kind == kind) {
                allImpl(map, r->first, r->second, out, mode, kind);
            } else {
                out[r->first] = r->second;
            }
            break;
        case ClassRefs:
            if (info->isClass()) // for class/struct we want the references inserted directly regardless and also recursed
                out[r->first] = r->second;
            if (r->second->isClass()
                || r->second->kind == CXCursor_Destructor
                || r->second->kind == CXCursor_Constructor) { // if is a constructor/destructor/class reference we want to recurse it
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

    allImpl(map, loc, copy(), ret, mode, kind);
    return ret;
}

SymbolMap CursorInfo::virtuals(const Location &loc, const SymbolMap &map) const
{
    SymbolMap ret;
    ret[loc] = copy();
    const SymbolMap s = (kind == CXCursor_CXXMethod ? allReferences(loc, map) : targetInfos(map));
    for (auto it = s.begin(); it != s.end(); ++it) {
        if (it->second->kind == kind)
            ret[it->first] = it->second;
    }
    return ret;
}

SymbolMap CursorInfo::declarationAndDefinition(const Location &loc, const SymbolMap &map) const
{
    SymbolMap cursors;
    cursors[loc] = copy();

    Location l;
    const std::shared_ptr<CursorInfo> t = bestTarget(map, &l);

    if (t->kind == kind)
        cursors[l] = t;
    return cursors;
}

String CursorInfo::displayName() const
{
    switch (kind) {
    case CXCursor_FunctionTemplate:
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor: {
        const int end = symbolName.indexOf('(');
        if (end != -1)
            return symbolName.left(end);
        break; }
    case CXCursor_FieldDecl: {
        int colon = symbolName.indexOf(':');
        if (colon != -1) {
            const int end = colon + 2;
            while (colon > 0 && RTags::isSymbol(symbolName.at(colon - 1)))
                --colon;
            return symbolName.left(colon + 1) + symbolName.mid(end);
        }
        break; }
    default:
        break;
    }
    return symbolName;
}
std::shared_ptr<CursorInfo> CursorInfo::copy() const
{
    return std::shared_ptr<CursorInfo>(std::make_shared<CursorInfo>(*this));
}
