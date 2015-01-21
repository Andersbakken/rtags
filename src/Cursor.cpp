
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

#include "Cursor.h"
#include "RTags.h"
#include "RTagsClang.h"

uint16_t Cursor::targetsValue() const
{
    return RTags::createTargetsValue(kind, isDefinition());
}


String Cursor::toString(unsigned cursorInfoFlags, unsigned keyFlags) const
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

    // if (!targets.isEmpty() && !(cursorInfoFlags & IgnoreTargets)) {
    //     ret.append("Targets:\n");
    //     for (auto tit = targets.begin(); tit != targets.end(); ++tit) {
    //         const Location &l = *tit;
    //         ret.append(String::format<128>("    %s\n", l.key(keyFlags).constData()));
    //     }
    // }

    // if (!references.isEmpty() && !(cursorInfoFlags & IgnoreReferences)) {
    //     ret.append("References:\n");
    //     for (auto rit = references.begin(); rit != references.end(); ++rit) {
    //         const Location &l = *rit;
    //         ret.append(String::format<128>("    %s\n", l.key(keyFlags).constData()));
    //     }
    // }
    return ret;
}

String Cursor::kindSpelling(uint16_t kind)
{
    return RTags::eatString(clang_getCursorKindSpelling(static_cast<CXCursorKind>(kind)));
}
