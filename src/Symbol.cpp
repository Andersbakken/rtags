/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <functional>
#include <limits>
#include <memory>
#include <string>

#include "RTags.h"
#include "Symbol.h"
#include "Project.h"
#include "FileMap.h"
#include "Location.h"
#include "clang-c/Index.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Set.h"
#include "rct/String.h"
#include "rct/Value.h"

uint16_t Symbol::targetsValue() const
{
    return RTags::createTargetsValue(kind, isDefinition());
}

static inline const char *linkageSpelling(CXLinkageKind kind)
{
    switch (kind) {
    case CXLinkage_Invalid: return "";
    case CXLinkage_NoLinkage: return "Linkage: No Linkage";
    case CXLinkage_Internal: return "Linkage: Internal";
    case CXLinkage_UniqueExternal: return "Linkage: Unique External";
    case CXLinkage_External: return "Linkage: External";
    }
    return "";
}

static String sourceCode(const Path &path, int startLine, int startColumn, int endLine, int endColumn)
{
    assert(startLine != -1);
    assert(startColumn != -1);
    assert(endLine != -1);
    assert(endColumn != -1);
    assert(startLine < endLine || (startLine == endLine && startColumn < endColumn));
    const String source = path.readAll(1024 * 1024);
    if (source.isEmpty()) {
        return String();
    }

    const size_t start = RTags::findOffset(startLine, startColumn, source);
    if (start == String::npos) {
        return String();
    }

    if (endLine == startLine) {
        return source.mid(start, endColumn - startColumn);
    }
    const size_t end = RTags::findOffset(endLine, endColumn, source);
    if (end == String::npos) {
        return String();
    }
    return source.mid(start, end - start);
}

String Symbol::toString(const std::shared_ptr<Project> &project,
                        const Flags<ToStringFlag> cursorInfoFlags,
                        Flags<Location::ToStringFlag> locationToStringFlags,
                        const Set<String> &pieceFilters) const
{
    auto filterPiece = [&pieceFilters](const char *name) { return pieceFilters.isEmpty() || pieceFilters.contains(name); };
    auto properties = [this, &filterPiece]() -> String {
        List<String> ret;
        if (isDefinition() && filterPiece("definition"))
            ret << "Definition";
        if (isContainer() && filterPiece("container"))
            ret << "Container";
        if ((flags & PureVirtualMethod) == PureVirtualMethod && filterPiece("purevirtual"))
            ret << "Pure Virtual";
        if (flags & VirtualMethod && filterPiece("virtual"))
            ret << "Virtual";

        if (flags & ConstMethod) {
            if (filterPiece("constmethod"))
                ret << "ConstMethod";
        } else if (flags & StaticMethod && filterPiece("static")) {
            ret << "Static";
        }

        if (flags & Variadic && filterPiece("variadic"))
            ret << "Variadic";
        if (flags & Auto && filterPiece("auto"))
            ret << "Auto";

        if (flags & MacroExpansion && filterPiece("macroexpansion"))
            ret << "MacroExpansion";
        if (flags & TemplateSpecialization && filterPiece("templatespecialization"))
            ret << "TemplateSpecialization";
        if (flags & TemplateReference && filterPiece("templatereference"))
            ret << "TemplateReference";

        if (ret.isEmpty())
            return String();
        return String::join(ret, ' ') + '\n';
    };

    List<String> bases;
    List<String> args;

    if (project) {
        if (filterPiece("baseclasses")) {
            for (const auto &base : baseClasses) {
                bool found = false;
                for (const auto &sym : project->findByUsr(base, location.fileId(), Project::ArgDependsOn)) {
                    bases << sym.symbolName;
                    found = true;
                    break;
                }
                if (!found) {
                    bases << base;
                }
            }
        }
        if (filterPiece("arguments")) {
            for (const auto &arg : arguments) {
                const String symName = project->findSymbol(arg.cursor).symbolName;
                if (!symName.isEmpty()) {
                    args << symName;
                } else {
                    args << arg.cursor.toString(locationToStringFlags & ~Location::ShowContext);
                }
            }
        }
    } else if (filterPiece("baseClasses")) {
        bases = baseClasses;
    }

    String ret;
    auto writePiece = [&ret, &filterPiece](const char *key, const char *filter, const String &piece) {
        if (piece.isEmpty())
            return;
        if (!filterPiece(filter))
            return;
        if (key && strlen(key))
            ret << key << ": ";
        ret << piece << "\n";
    };
    writePiece(nullptr, "location", location.toString(locationToStringFlags));
    writePiece("SymbolName", "symbolname", symbolName);
    writePiece("Kind", "kind", kindSpelling());
    if (filterPiece("type")) {
        if (!typeName.isEmpty()) {
            ret += "Type: " + typeName + "\n";
        } else if (type != CXType_Invalid) {
            ret += "Type: " + RTags::eatString(clang_getTypeKindSpelling(type)) + "\n";
        }
    }
    writePiece("SymbolLength", "symbollength", std::to_string(symbolLength));

    if (startLine != -1)
        writePiece("Range", "range", String::format<32>("%d:%d-%d:%d", startLine, startColumn, endLine, endColumn));

#if CINDEX_VERSION_MINOR > 1
    if (kind == CXCursor_EnumConstantDecl)
        writePiece("Enum Value", "enumvalue",
                   String::format<32>("%lld/0x%0llx", static_cast<long long>(enumValue), static_cast<long long>(enumValue)));

    if (isDefinition() && RTags::isFunction(kind))
        writePiece("Stack cost", "stackcost", std::to_string(stackCost));
#endif
    writePiece(nullptr, "linkage", linkageSpelling(linkage));
    ret += properties();
    writePiece("Usr", "usr", usr);
    if (size)
        writePiece("sizeof", "sizeof", std::to_string(size));
    if (fieldOffset >= 0)
        writePiece("Field offset (bits/bytes)", "fieldoffset",
                   String::format<32>("%d/%d", fieldOffset, fieldOffset / 8));
    if (alignment >= 0)
        writePiece("Alignment", "alignment", std::to_string(alignment));
    if (!args.isEmpty())
        writePiece("Arguments", "arguments", String::join(args, ", "));
    if (!bases.isEmpty())
        writePiece("Base classes", "baseclasses", String::join(bases, ", "));
    writePiece("Brief comment", "briefcomment", briefComment);
    writePiece("XML comment", "xmlcomment", xmlComment);

    if ((cursorInfoFlags & IncludeParents && filterPiece("parent"))
        || (cursorInfoFlags & (IncludeContainingFunction) && filterPiece("cf"))
        || (cursorInfoFlags & (IncludeContainingFunctionLocation) && filterPiece("cfl"))) {
        auto syms = project->openSymbols(location.fileId());
        uint32_t idx = -1;
        if (syms) {
            idx = syms->lowerBound(location);
            if (idx == std::numeric_limits<uint32_t>::max()) {
                idx = syms->count() - 1;
            }
        }
        const unsigned int line = location.line();
        const unsigned int column = location.column();
        while (idx-- > 0) {
            const Symbol s = syms->valueAt(idx);
            if (s.isDefinition()
                && s.isContainer()
                && comparePosition(line, column, s.startLine, s.startColumn) >= 0
                && comparePosition(line, column, s.endLine, s.endColumn) <= 0) {
                if (cursorInfoFlags & IncludeContainingFunctionLocation)
                    writePiece("Containing function location", "cfl", s.location.toString(locationToStringFlags));
                if (cursorInfoFlags & IncludeContainingFunction)
                    writePiece("Containing function", "cf", s.symbolName);
                if (cursorInfoFlags & IncludeParents)
                    writePiece("Parent", "parent", s.location.toString(locationToStringFlags)); // redundant, this is a mess
                break;
            }
        }
    }

    if (cursorInfoFlags & IncludeTargets && project && filterPiece("targets")) {
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

    if (cursorInfoFlags & IncludeReferences && project && !isReference() && filterPiece("references")) {
        const auto references = project->findCallers(*this);
        if (references.size()) {
            ret.append("References:\n");
            for (const auto &r : references) {
                ret.append(String::format<128>("    %s\n", r.location.toString(locationToStringFlags).constData()));
            }
        }
    }

    if (cursorInfoFlags & IncludeSourceCode && (endLine > startLine || (endLine == startLine && endColumn > startColumn))) {
        const Path path = location.path();
        String source = sourceCode(path, startLine, startColumn, endLine, endColumn);
        if (!source.isEmpty()) {
            ret.append(String::format<1024>("\nSource code: %s:%d:%d-%d:%d\n",
                                            path.constData(), startLine, startColumn,
                                            endLine, endColumn));
            ret.append(source);
            ret.append('\n');
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
                      Flags<Location::ToStringFlag> locationToStringFlags,
                      const Set<String> &pieceFilters) const
{
    auto filterPiece = [&pieceFilters](const char *name) { return pieceFilters.isEmpty() || pieceFilters.contains(name); };
    std::function<Value(const Symbol &, Flags<ToStringFlag>)> toValue = [&](const Symbol &symbol, Flags<ToStringFlag> f) {
        Value ret;
        auto formatLocation = [locationToStringFlags,&filterPiece, &ret](Location loc, const char *key, const char *ctxKey,
                                                                         const char *keyFilter = nullptr,
                                                                         const char *ctxKeyFilter = nullptr,
                                                                         Value *val = nullptr) {
            if (!val)
                val = &ret;
            if (filterPiece(keyFilter ? keyFilter : key))
                (*val)[key] = loc.toString(locationToStringFlags & ~Location::ShowContext);
            if (locationToStringFlags & Location::ShowContext && filterPiece(ctxKeyFilter ? ctxKeyFilter : ctxKey)) {
                (*val)[ctxKey] = loc.context(locationToStringFlags);
            }
        };
        if (!symbol.location.isNull()) {
            formatLocation(symbol.location, "location", "context");
        }
        if (!symbol.isNull()) {
            if (symbol.argumentUsage.index != String::npos) {
                formatLocation(symbol.argumentUsage.invocation, "invocation", "invocationContext", nullptr, "invocationcontext");
                if (filterPiece("invokedfunction"))
                    ret["invokedFunction"] = symbol.argumentUsage.invokedFunction.toString(locationToStringFlags);
                formatLocation(symbol.argumentUsage.argument.location, "functionArgumentLocation", "functionArgumentLocationContext",
                               "functionargumentlocation", "functionargumentlocationcontext");
                if (filterPiece("functionargumentcursor"))
                    ret["functionArgumentCursor"] = symbol.argumentUsage.argument.cursor.toString(locationToStringFlags);
                if (filterPiece("functionargumentlength"))
                    ret["functionArgumentLength"] = symbol.argumentUsage.argument.length;
                if (filterPiece("argumentindex"))
                    ret["argumentIndex"] = symbol.argumentUsage.index;
            }
            if (!symbol.symbolName.isEmpty() && filterPiece("symbolname"))
                ret["symbolName"] = symbol.symbolName;
            if (!symbol.usr.isEmpty() && filterPiece("usr"))
                ret["usr"] = symbol.usr;
            if (filterPiece("type")) {
                if (!symbol.typeName.isEmpty()) {
                    ret["type"] = symbol.typeName;
                } else if (symbol.type != CXType_Invalid) {
                    String str;
                    Log(&str) << symbol.type;
                    ret["type"] = str;
                }
            }

            if (!symbol.baseClasses.isEmpty() && filterPiece("baseclasses"))
                ret["baseClasses"] = symbol.baseClasses;
            if (!symbol.arguments.isEmpty() && filterPiece("arguments")) {
                Value args;
                for (const auto &arg : symbol.arguments) {
                    Value a;
                    formatLocation(arg.location, "location", "context", "arguments", "arguments", &a);
                    formatLocation(arg.location, "cursor", "cursorContext", "arguments", "arguments", &a);
                    a["length"] = arg.length;
                    args.push_back(a);
                }
                ret["arguments"] = args;
            }
            if (filterPiece("symbollength"))
                ret["symbolLength"] = symbol.symbolLength;
            if (filterPiece("kind")) {
                String str;
                Log(&str) << symbol.kind;
                ret["kind"] = str;
            }
            if (filterPiece("linkage")) {
                String str;
                Log(&str) << symbol.linkage;
                ret["linkage"] = str;
            }

            if (!symbol.briefComment.isEmpty() && filterPiece("briefcomment"))
                ret["briefComment"] = symbol.briefComment;
            if (!symbol.xmlComment.isEmpty() && filterPiece("xmlcomment"))
                ret["xmlComment"] = symbol.xmlComment;
            if (filterPiece("range")) {
                ret["startLine"] = symbol.startLine;
                ret["startColumn"] = symbol.startColumn;
                ret["endLine"] = symbol.endLine;
                ret["endColumn"] = symbol.endColumn;
            }
            if (symbol.size && filterPiece("sizeof"))
                ret["sizeof"] = symbol.size;
            if (symbol.fieldOffset >= 0 && filterPiece("fieldoffset"))
                ret["fieldOffset"] = symbol.fieldOffset;
            if (symbol.alignment >= 0 && filterPiece("alignment"))
                ret["alignment"] = symbol.alignment;
            if (symbol.kind == CXCursor_EnumConstantDecl && filterPiece("enumvalue"))
                ret["enumValue"] = symbol.enumValue;
            if (symbol.isDefinition()) {
                if (filterPiece("definition"))
                    ret["definition"] = true;
                if (RTags::isFunction(symbol.kind) && filterPiece("stackcost"))
                    ret["stackCost"] = symbol.stackCost;
            } else if (symbol.isReference() && filterPiece("reference")) {
                ret["reference"] = true;
            }
            if (symbol.isContainer() && filterPiece("container"))
                ret["container"] = true;
            if ((symbol.flags & Symbol::PureVirtualMethod) == Symbol::PureVirtualMethod && filterPiece("purevirtual"))
                ret["purevirtual"] = true;
            if (symbol.flags & Symbol::VirtualMethod && filterPiece("virtual"))
                ret["virtual"] = true;
            if (symbol.flags & Symbol::ConstMethod && filterPiece("constmethod"))
                ret["constmethod"] = true;
            if (symbol.flags & Symbol::StaticMethod && filterPiece("staticmethod"))
                ret["staticmethod"] = true;
            if (symbol.flags & Symbol::Variadic && filterPiece("variadic"))
                ret["variadic"] = true;
            if (symbol.flags & Symbol::Auto && filterPiece("auto"))
                ret["auto"] = true;
            if (symbol.flags & Symbol::MacroExpansion && filterPiece("macroexpansion"))
                ret["macroexpansion"] = true;
            if (symbol.flags & Symbol::TemplateSpecialization && filterPiece("templatespecialization"))
                ret["templatespecialization"] = true;
            if (symbol.flags & Symbol::TemplateReference && filterPiece("templatereference"))
                ret["templatereference"] = true;
            if (f & IncludeTargets) {
                const auto targets = project->findTargets(symbol);
                if (!targets.isEmpty() && filterPiece("targets")) {
                    Value t;
                    for (const auto &target : targets) {
                        t.push_back(toValue(target, NullFlags));
                    }
                    ret["targets"] = t;
                }
            }
            if (f & IncludeReferences) {
                const auto references = project->findCallers(symbol);
                if (!references.isEmpty() && filterPiece("references")) {
                    Value r;
                    for (const auto &ref : references) {
                        r.push_back(toValue(ref, NullFlags));
                    }
                    ret["references"] = r;
                }
            }
            if (f & IncludeBaseClasses && filterPiece("baseclasses")) {
                List<Value> b;
                for (const auto &base : symbol.baseClasses) {
                    for (const Symbol &s : project->findByUsr(base, symbol.location.fileId(), Project::ArgDependsOn)) {
                        b.append(toValue(s, NullFlags));
                        break;
                    }
                }
                if (!baseClasses.isEmpty()) {
                    ret["baseClasses"] = b;
                }
            }

            if ((f & IncludeParents && filterPiece("parent"))
                || (f & (IncludeContainingFunction) && filterPiece("cf"))
                || (f & (IncludeContainingFunctionLocation) && (filterPiece("cfl") || filterPiece("cflcontext")))) {
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
                        if (f & IncludeContainingFunctionLocation) {
                            formatLocation(s.location, "cfl", "cflcontext");
                        }
                        if (f & IncludeContainingFunction && filterPiece("cf"))
                            ret["cf"] = s.symbolName;
                        if (f & IncludeParents && filterPiece("parent"))
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
