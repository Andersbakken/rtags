
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

#include "RTags.h"
#include "Symbol.h"
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
                        Flags<Location::ToStringFlag> locationToStringFlags,
                        const std::shared_ptr<Project> &project) const
{
    auto properties = [this]()
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

            if (flags & MacroExpansion)
                ret << "MacroExpansion";
            if (flags & TemplateSpecialization)
                ret << "TemplateSpecialization";

            if (ret.isEmpty())
                return String();
            String joined = String::join(ret, ' ');
            joined += '\n';
            return joined;
        };

    List<String> bases;
    List<String> args;
    if (project) {
        for (const auto &base : baseClasses) {
            bool found = false;
            for (const auto &sym : project->findByUsr(base, location.fileId(), Project::ArgDependsOn, location)) {
                bases << sym.symbolName;
                found = true;
                break;
            }
            if (!found) {
                bases << base;
            }
        }
        for (const auto &arg : arguments) {
            const String symbolName = project->findSymbol(arg.first).symbolName;
            if (!symbolName.isEmpty()) {
                args << symbolName;
            } else {
                args << arg.first.toString(locationToStringFlags & ~Location::ShowContext);
            }
        }
    } else {
        bases = baseClasses;
    }

    auto printTypeName = [this]() {
        String str;
        if (!typeName.isEmpty()) {
            str = typeName;
        } else if (type != CXType_Invalid) {
            str = RTags::eatString(clang_getTypeKindSpelling(type));
        } else {
            return String();
        }
        return String::format<128>("Type: %s\n", str.constData());
    };

    String ret = String::format<1024>("SymbolName: %s\n"
                                      "Kind: %s\n"
                                      "%s" // type
                                      "SymbolLength: %u\n"
                                      "%s" // range
                                      "%s" // enumValue/stackCost
                                      "%s" // linkage
                                      "%s" // properties
                                      "%s" // usr
                                      "%s" // sizeof
                                      "%s" // fieldoffset
                                      "%s" // alignment
                                      "%s" // arguments
                                      "%s" // baseclasses
                                      "%s" // briefComment
                                      "%s", // xmlComment
                                      symbolName.constData(),
                                      kindSpelling().constData(),
                                      printTypeName().constData(),
                                      symbolLength,
                                      startLine != -1 ? String::format<32>("Range: %d:%d-%d:%d\n", startLine, startColumn, endLine, endColumn).constData() : "",
#if CINDEX_VERSION_MINOR > 1
                                      (kind == CXCursor_EnumConstantDecl
                                       ? String::format<32>("Enum Value: %lld/0x%0llx\n",
                                                            static_cast<long long>(enumValue),
                                                            static_cast<long long>(enumValue)).constData()
                                       : (isDefinition() && RTags::isFunction(kind)
                                          ? String::format<32>("Stack cost: %d\n", stackCost).constData()
                                          : "")),
#else
                                      "",
#endif
                                      linkageSpelling(linkage),
                                      properties().constData(),
                                      usr.isEmpty() ? "" : String::format<64>("Usr: %s\n", usr.constData()).constData(),
                                      size > 0 ? String::format<16>("sizeof: %d\n", size).constData() : "",
                                      fieldOffset >= 0 ? String::format<32>("field offset (bits/bytes): %d/%d\n", fieldOffset, fieldOffset / 8).constData() : "",
                                      alignment >= 0 ? String::format<32>("alignment (bytes): %d\n", alignment).constData() : "",
                                      args.isEmpty() ? "" : String::format<1024>("Arguments: %s\n", String::join(args, ", ").constData()).constData(),
                                      bases.isEmpty() ? "" : String::format<64>("BaseClasses: %s\n", String::join(bases, ", ").constData()).constData(),
                                      briefComment.isEmpty() ? "" : String::format<1024>("Brief comment: %s\n", briefComment.constData()).constData(),
                                      xmlComment.isEmpty() ? "" : String::format<16384>("Xml comment: %s\n", xmlComment.constData()).constData());
    if (!(cursorInfoFlags & IgnoreTargets) && project) {
        const auto targets = project->findTargets(*this);
        if (targets.size()) {
            ret.append("Targets:\n");
            auto best = RTags::bestTarget(targets);
            ret.append(String::format<128>("    %s\n", best.location.toString(locationToStringFlags).constData()));

            for (const auto &tit : targets) {
                if (tit.location != best.location)
                    ret.append(String::format<128>("    %s\n", tit.location.toString(locationToStringFlags).constData()));
            }
        }
    }

    if (!(cursorInfoFlags & IgnoreReferences) && project && !isReference()) {
        const auto references = project->findCallers(*this);
        if (references.size()) {
            ret.append("References:\n");
            for (const auto &r : references) {
                ret.append(String::format<128>("    %s\n", r.location.toString(locationToStringFlags).constData()));
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
    return RTags::isReference(kind) || (linkage == CXLinkage_External && !isDefinition() && !RTags::isFunction(kind));
}

bool Symbol::isContainer() const
{
    return RTags::isContainer(kind);
}
