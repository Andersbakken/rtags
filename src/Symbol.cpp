
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
            const String symName = project->findSymbol(arg.cursor).symbolName;
            if (!symName.isEmpty()) {
                args << symName;
            } else {
                args << arg.cursor.toString(locationToStringFlags & ~Location::ShowContext);
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

    String ret = String::format<1024>("%s\n"
                                      "SymbolName: %s\n"
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
                                      location.toString(locationToStringFlags).constData(),
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
                                      xmlComment.isEmpty() ? "" : String::format<16384>("XML comment: %s\n", xmlComment.constData()).constData());
    if (cursorInfoFlags & IncludeTargets && project) {
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

    if (cursorInfoFlags & IncludeReferences && project && !isReference()) {
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

Value Symbol::toValue(const std::shared_ptr<Project> &project,
                      Flags<ToStringFlag> toStringFlags,
                      Flags<Location::ToStringFlag> locationToStringFlags) const
{
    std::function<Value(const Symbol &, Flags<ToStringFlag>)> toValue = [&](const Symbol &symbol, Flags<ToStringFlag> f) {
        Value ret;
        if (!symbol.isNull()) {
            ret["location"] = symbol.location.toString(locationToStringFlags);
            if (symbol.argumentUsage.index != String::npos) {
                ret["invocation"] = symbol.argumentUsage.invocation.toString(locationToStringFlags);
                ret["invokedFunction"] = symbol.argumentUsage.invokedFunction.toString(locationToStringFlags);
                ret["functionArgumentLocation"] = symbol.argumentUsage.argument.location.toString(locationToStringFlags);
                ret["functionArgumentCursor"] = symbol.argumentUsage.argument.cursor.toString(locationToStringFlags);
                ret["functionArgumentLength"] = symbol.argumentUsage.argument.length;
                ret["argumentIndex"] = symbol.argumentUsage.index;
            }
            if (!symbol.symbolName.isEmpty())
                ret["symbolName"] = symbol.symbolName;
            if (!symbol.usr.isEmpty())
                ret["usr"] = symbol.usr;
            if (!symbol.typeName.isEmpty()) {
                ret["type"] = symbol.typeName;
            } else if (symbol.type != CXType_Invalid) {
                String str;
                Log(&str) << symbol.type;
                ret["type"] = str;
            }

            if (!symbol.baseClasses.isEmpty())
                ret["baseClasses"] = symbol.baseClasses;
            if (!symbol.arguments.isEmpty()) {
                Value args;
                for (const auto &arg : symbol.arguments) {
                    Value a;
                    a["location"] = arg.location.toString(locationToStringFlags);
                    a["cursor"] = arg.cursor.toString(locationToStringFlags);
                    a["length"] = arg.length;
                    args.push_back(a);
                }
                ret["arguments"] = args;
            }
            ret["symbolLength"] = symbol.symbolLength;
            {
                String str;
                Log(&str) << symbol.kind;
                ret["kind"] = str;
            }
            {
                String str;
                Log(&str) << symbol.linkage;
                ret["linkage"] = str;
            }

            if (!symbol.briefComment.isEmpty())
                ret["briefComment"] = symbol.briefComment;
            if (!symbol.xmlComment.isEmpty())
                ret["xmlComment"] = symbol.xmlComment;
            ret["startLine"] = symbol.startLine;
            ret["startColumn"] = symbol.startColumn;
            ret["endLine"] = symbol.endLine;
            ret["endColumn"] = symbol.endColumn;
            if (symbol.size > 0)
                ret["sizeof"] = symbol.size;
            if (symbol.fieldOffset > 0)
                ret["fieldOffset"] = symbol.fieldOffset;
            if (symbol.alignment > 0)
                ret["alignment"] = symbol.alignment;
            if (symbol.kind == CXCursor_EnumConstantDecl)
                ret["enumValue"] = symbol.enumValue;
            if (symbol.isDefinition()) {
                ret["definition"] = true;
                if (RTags::isFunction(symbol.kind))
                    ret["stackCost"] = symbol.stackCost;
            } else if (symbol.isReference()) {
                ret["reference"] = true;
            }
            if (symbol.isContainer())
                ret["container"] = true;
            if ((symbol.flags & Symbol::PureVirtualMethod) == Symbol::PureVirtualMethod)
                ret["purevirtual"] = true;
            if (symbol.flags & Symbol::VirtualMethod)
                ret["virtual"] = true;
            if (symbol.flags & Symbol::ConstMethod)
                ret["constmethod"] = true;
            if (symbol.flags & Symbol::StaticMethod)
                ret["staticmethod"] = true;
            if (symbol.flags & Symbol::Variadic)
                ret["variadic"] = true;
            if (symbol.flags & Symbol::Auto)
                ret["auto"] = true;
            if (symbol.flags & Symbol::AutoRef)
                ret["autoref"] = true;
            if (symbol.flags & Symbol::MacroExpansion)
                ret["macroexpansion"] = true;
            if (symbol.flags & Symbol::TemplateSpecialization)
                ret["templatespecialization"] = true;
            if (f & IncludeTargets) {
                const auto targets = project->findTargets(symbol);
                if (!targets.isEmpty()) {
                    Value t;
                    for (const auto &target : targets) {
                        t.push_back(toValue(target, NullFlags));
                    }
                    ret["targets"] = t;
                }
            }
            if (f & IncludeReferences) {
                const auto references = project->findCallers(symbol);
                if (!references.isEmpty()) {
                    Value r;
                    for (const auto &ref : references) {
                        r.push_back(toValue(ref, NullFlags));
                    }
                    ret["references"] = r;
                }
            }
            if (f & IncludeBaseClasses) {
                List<Value> b;
                for (const auto &base : symbol.baseClasses) {
                    for (const Symbol &s : project->findByUsr(base, symbol.location.fileId(), Project::ArgDependsOn, symbol.location)) {
                        b.append(toValue(s, NullFlags));
                        break;
                    }
                }
                if (!baseClasses.isEmpty()) {
                    ret["baseClasses"] = b;
                }
            }

            if (f & IncludeParents) {
                auto syms = project->openSymbols(symbol.location.fileId());
                uint32_t idx = -1;
                if (syms) {
                    idx = syms->lowerBound(symbol.location);
                    if (idx == std::numeric_limits<uint32_t>::max()) {
                        idx = syms->count() - 1;
                    }
                }
                const unsigned int line = symbol.location.line();
                const unsigned int column = symbol.location.column();
                while (idx-- > 0) {
                    const Symbol s = syms->valueAt(idx);
                    if (s.isDefinition()
                        && s.isContainer()
                        && comparePosition(line, column, s.startLine, s.startColumn) >= 0
                        && comparePosition(line, column, s.endLine, s.endColumn) <= 0) {
                        ret["parent"] = toValue(s, IncludeParents);
                        break;
                    }
                }
            }
        }
        return ret;
    };
    return toValue(*this, toStringFlags);
}
