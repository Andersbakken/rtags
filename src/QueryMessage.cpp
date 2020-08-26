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

#include "QueryMessage.h"

#include <assert.h>
#include <unordered_map>

#include "rct/Serializer.h"
#include "Symbol.h"
#include "clang-c/Index.h"
#include "rct/Rct.h"

QueryMessage::QueryMessage(Type type)
    : RTagsMessage(MessageId), mType(type), mMax(-1), mMaxDepth(-1), mMinLine(-1), mMaxLine(-1), mBuildIndex(0), mTerminalWidth(-1)
{
}

void QueryMessage::encode(Serializer &serializer) const
{
    serializer << mCommandLine << mQuery << mCodeCompletePrefix << mType << mFlags << mMax
               << mMaxDepth << mMinLine << mMaxLine << mBuildIndex << mPathFilters << mKindFilters
               << mCurrentFile << mUnsavedFiles << mTerminalWidth;
}

void QueryMessage::decode(Deserializer &deserializer)
{
    deserializer >> mCommandLine >> mQuery >> mCodeCompletePrefix >> mType >> mFlags >> mMax
                 >> mMaxDepth >> mMinLine >> mMaxLine >> mBuildIndex >> mPathFilters >> mKindFilters
                 >> mCurrentFile >> mUnsavedFiles >> mTerminalWidth;
}

Flags<Location::ToStringFlag> QueryMessage::locationToStringFlags(Flags<Flag> queryFlags)
{
    Flags<Location::ToStringFlag> ret;
    if (!(queryFlags & NoContext))
        ret |= Location::ShowContext;
    if (queryFlags & NoColor)
        ret |= Location::NoColor;
    if (queryFlags & AbsolutePath)
        ret |= Location::AbsolutePath;
    return ret;
}

Match QueryMessage::match() const
{
    Flags<Match::Flag> flags = Match::Flag_StringMatch;
    if (mFlags & MatchRegex)
        flags |= Match::Flag_Regex;

    return Match(mQuery, flags);
}

QueryMessage::Flag QueryMessage::flagFromString(const String &string)
{
    if (string == "no-context") {
        return NoContext;
    } else if (string == "filter-system-includes") {
        return FilterSystemIncludes;
    } else if (string == "strip-parentheses") {
        return StripParentheses;
    } else if (string == "all-references") {
        return AllReferences;
    } else if (string == "reverse-sort") {
        return ReverseSort;
    } else if (string == "elisp") {
        return Elisp;
    } else if (string == "match-regexp") {
        return MatchRegex;
    } else if (string == "match-case-insensitive") {
        return MatchCaseInsensitive;
    } else if (string == "find-virtuals") {
        return FindVirtuals;
    } else if (string == "silent") {
        return Silent;
    } else if (string == "absolute-path") {
        return AbsolutePath;
    } else if (string == "find-file-prefer-exact") {
        return FindFilePreferExact;
    } else if (string == "symbol-info-include-parents") {
        return SymbolInfoIncludeParents;
    } else if (string == "symbol-info-include-targets") {
        return SymbolInfoIncludeTargets;
    } else if (string == "symbol-info-include-references") {
        return SymbolInfoIncludeReferences;
    } else if (string == "symbol-info-include-base-classes") {
        return SymbolInfoIncludeBaseClasses;
    } else if (string == "declaration-only") {
        return DeclarationOnly;
    } else if (string == "definiton-only") {
        return DefinitionOnly;
    } else if (string == "containing-function") {
        return ContainingFunction;
    } else if (string == "cursor-kind") {
        return CursorKind;
    } else if (string == "display-name") {
        return DisplayName;
    } else if (string == "compilation-flags-only") {
        return CompilationFlagsOnly;
    } else if (string == "compilation-flags-split-line") {
        return CompilationFlagsSplitLine;
    } else if (string == "dump-include-headers") {
        return DumpIncludeHeaders;
    } else if (string == "silent-query") {
        return SilentQuery;
    } else if (string == "synchronous-completions") {
        return SynchronousCompletions;
    } else if (string == "no-sort-references-by-input") {
        return NoSortReferencesByInput;
    } else if (string == "has-location") {
        return HasLocation;
    } else if (string == "wildcard-symbol-names") {
        return WildcardSymbolNames;
    } else if (string == "no-color") {
        return NoColor;
    } else if (string == "target-usrs") {
        return TargetUsrs;
    }
    return NoFlag;
}

bool QueryMessage::KindFilters::filter(const Symbol &symbol) const
{
    if (isEmpty())
        return true;

    String spelling = Symbol::kindSpelling(symbol.kind).toLower();
    spelling.remove(' ');
    auto match = [&spelling, &symbol](const Map<String, Flags<DefinitionType> > &map, bool hasWildcardsOrCategories) {
        auto it = map.find(spelling);
        auto matchDefinition = [&symbol](Flags<DefinitionType> f) {
            f &= Definition|NotDefinition;
            switch (f.cast<int>()) {
            case Definition:
                if (symbol.isDefinition())
                    return true;
                break;
            case NotDefinition:
                if (!symbol.isDefinition())
                    return true;
                break;
            default:
                assert(f == (Definition|NotDefinition));
                return true;
            }
            return false;
        };
        if (it != map.end() && matchDefinition(it->second)) {
            return true;
        }
        if (hasWildcardsOrCategories) {
            for (const auto &pair : map) {
                if (pair.second & Wildcard) {
                    if (matchDefinition(pair.second) && Rct::wildCmp(pair.first.constData(), spelling.constData())) {
                        return true;
                    }
                }  else if (pair.second & Category && matchDefinition(pair.second)) {
                    if (pair.first == "references") {
                        if (symbol.isReference())
                            return true;
                    } else if (pair.first == "statements") {
                        if (clang_isStatement(symbol.kind))
                            return true;
                    } else if (pair.first == "declarations") {
                        if (clang_isDeclaration(symbol.kind))
                            return true;
                    } else if (pair.first == "expressions") {
                        if (clang_isExpression(symbol.kind))
                            return true;
                    } else if (pair.first == "attributes") {
                        if (clang_isAttribute(symbol.kind))
                            return true;
                    } else if (pair.first == "preprocessing") {
                        if (clang_isPreprocessing(symbol.kind))
                            return true;
                    } else {
                        assert(0);
                    }
                }
            }
        }
        return false;
    };
    if (!out.isEmpty() && match(out, flags & (OutHasWildcards|OutHasCategories))) {
        return false;
    }
    if (in.isEmpty() || match(in, flags & (InHasWildcards|InHasCategories))) {
        return true;
    } else {
        return false;
    }
}

void QueryMessage::KindFilters::insert(const String &arg)
{
    for (String a : arg.toLower().split(',', String::SkipEmpty)) {
        Flags<DefinitionType> f = Definition|NotDefinition;
        Map<String, Flags<DefinitionType> > *target = &in;
        const bool hasWildCard = a.contains("?") || a.contains("*");
        if (hasWildCard)
            f |= Wildcard;

        if (a.endsWith('+')) {
            a.chop(1);
            f &= ~NotDefinition;
        } else if (a.endsWith('-')) {
            a.chop(1);
            f &= ~Definition;
        }

        const bool o = a.startsWith('-');
        if (o) {
            target = &out;
            if (hasWildCard)
                flags |= OutHasWildcards;
            a.remove(0, 1);
        } else if (hasWildCard) {
            flags |= InHasWildcards;
        }
        if (a == "references" || a == "statements" || a == "declarations"
            || a == "expressions" || a == "attributes" || a == "preprocessing") {
            f |= Category;
            flags |= (o ? OutHasCategories : InHasCategories);
        }
        (*target)[a] |= f;
    }
}
