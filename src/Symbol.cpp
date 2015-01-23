
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

#include "Symbol.h"
#include "RTags.h"
#include "RTagsClang.h"
#include "Project.h"

uint16_t Symbol::targetsValue() const
{
    return RTags::createTargetsValue(kind, isDefinition());
}


String Symbol::toString(unsigned cursorInfoFlags, unsigned keyFlags, const std::shared_ptr<Project> &project) const
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
    if (!(cursorInfoFlags & IgnoreTargets) && project) {
        auto targets = project->findTargets(*this);
        if (targets.size()) {
            ret.append("Targets:\n");
            auto best = RTags::bestTarget(targets);
            ret.append(String::format<128>("    %s\n", best.key(keyFlags).constData()));

            for (auto tit = targets.begin(); tit != targets.end(); ++tit) {
                const Location &l = tit->first;
                if (l != best)
                    ret.append(String::format<128>("    %s\n", l.key(keyFlags).constData()));
            }
        }
    }

    if (!(cursorInfoFlags & IgnoreReferences) && project) {
        auto references = project->findCallers(*this);
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
    return RTags::eatString(clang_getCursorKindSpelling(static_cast<CXCursorKind>(kind)));
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
