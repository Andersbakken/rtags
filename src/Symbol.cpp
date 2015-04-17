
/* This file is part of RTags (http://rtags.net).

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

#include "Symbol.h"
#include "RTags.h"
#include "RTagsClang.h"
#include "Project.h"

uint16_t Symbol::targetsValue() const
{
    return RTags::createTargetsValue(kind, isDefinition());
}

static inline const char *linkageSpelling(CXLinkageKind kind)
{
    switch (kind) {
    case CXLinkage_Invalid: return "";
    case CXLinkage_NoLinkage: return "Linkage: No Linkage\n";
    case CXLinkage_Internal: return "Linkage: Internal\n";
    case CXLinkage_UniqueExternal: return "Linkage: Unique External\n";
    case CXLinkage_External: return "Linkage: External\n";
    }
    return "";
}

String Symbol::toString(Flags<ToStringFlag> cursorInfoFlags,
                        Flags<Location::KeyFlag> keyFlags,
                        const std::shared_ptr<Project> &project) const
{
    static auto properties = [this]()
        {
            List<String> ret;
            if (isDefinition())
                ret << "Definition";
            if (isContainer())
                ret << "Container";
            if ((flags & PureVirtualMethod) == PureVirtualMethod) {
                ret << "Pure Virtual";
            } else if (flags & VirtualMethod) {
                ret << "Virtual";
            }
            if (flags & ConstMethod) {
                ret << "Const";
            } else if (flags & StaticMethod) {
                ret << "Static";
            }

            if (flags & Variadic)
                ret << "Variadic";
            if (flags & Auto)
                ret << "Auto";
            if (flags & AutoRef)
                ret << "AutoRef";
            if (ret.isEmpty())
                return String();
            String joined = String::join(ret, ' ');
            joined += '\n';
            return joined;
        };

    List<String> bases;
    if (project) {
        extern String findSymbolNameByUsr(const std::shared_ptr<Project> &, uint32_t, const String &);
        for (const auto &base : baseClasses) {
            const String usr = findSymbolNameByUsr(project, location.fileId(), base);
            if (!usr.isEmpty()) {
                bases << usr;
            }
        }
    } else {
        bases = baseClasses;
    }

    String ret = String::format<1024>("SymbolName: %s\n"
                                      "Kind: %s\n"
                                      "%s" // type
                                      "SymbolLength: %u\n"
                                      "%s" // range
                                      "%s" // enumValue
                                      "%s" // linkage
                                      "%s" // properties
                                      "%s" // usr
                                      "%s" // baseclasses
                                      "%s" // briefComment
                                      "%s", // xmlComment
                                      symbolName.constData(),
                                      kindSpelling().constData(),
                                      type == CXType_Invalid ? "" : String::format<128>("Type: %s\n", RTags::eatString(clang_getTypeKindSpelling(type)).constData()).constData(),
                                      symbolLength,
                                      startLine != -1 ? String::format<32>("Range: %d:%d-%d:%d\n", startLine, startColumn, endLine, endColumn).constData() : "",
#if CINDEX_VERSION_MINOR > 1
                                      kind == CXCursor_EnumConstantDecl ? String::format<32>("Enum Value: %lld\n", enumValue).constData() :
#endif
                                      "",
                                      linkageSpelling(linkage),
                                      properties().constData(),
                                      usr.isEmpty() ? "" : String::format<64>("Usr: %s\n", usr.constData()).constData(),
                                      bases.isEmpty() ? "" : String::format<64>("BaseClasses: %s\n", String::join(bases, ", ").constData()).constData(),
                                      briefComment.isEmpty() ? "" : String::format<1024>("Brief comment: %s\n", briefComment.constData()).constData(),
                                      xmlComment.isEmpty() ? "" : String::format<16384>("Xml comment: %s\n", xmlComment.constData()).constData());
    if (!(cursorInfoFlags & IgnoreTargets) && project) {
        extern Set<Symbol> findTargets(const std::shared_ptr<Project> &, const Symbol &);
        auto targets = findTargets(project, *this);
        if (targets.size()) {
            ret.append("Targets:\n");
            auto best = RTags::bestTarget(targets);
            ret.append(String::format<128>("    %s\n", best.location.key(keyFlags).constData()));

            for (const auto &tit : targets) {
                if (tit.location != best.location)
                    ret.append(String::format<128>("    %s\n", tit.location.key(keyFlags).constData()));
            }
        }
    }

    if (!(cursorInfoFlags & IgnoreReferences) && project && !isReference()) {
        extern Set<Symbol> findCallers(const std::shared_ptr<Project> &, const Symbol &);
        auto references = findCallers(project, *this);
        if (references.size()) {
            ret.append("References:\n");
            for (const auto &r : references) {
                ret.append(String::format<128>("    %s\n", r.location.key(keyFlags).constData()));
            }
        }
    }

    return ret;
}

String Symbol::kindSpelling(uint16_t kind)
{
    return kind ? RTags::eatString(clang_getCursorKindSpelling(static_cast<CXCursorKind>(kind))) : String("<none>");
}

String Symbol::displayName() const
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

bool Symbol::isReference() const
{
    return RTags::isReference(kind);
}

bool Symbol::isContainer() const
{
    return RTags::isContainer(kind);
}
