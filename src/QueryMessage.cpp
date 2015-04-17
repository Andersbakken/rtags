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

#include "QueryMessage.h"
#include "RTags.h"
#include <rct/Serializer.h>

QueryMessage::QueryMessage(Type type)
    : RTagsMessage(MessageId), mType(type), mMax(-1), mMinLine(-1), mMaxLine(-1), mBuildIndex(0)
{
}

void QueryMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mQuery << mType << mFlags << mMax
               << mMinLine << mMaxLine << mBuildIndex << mPathFilters << mCurrentFile
               << mUnsavedFiles;
}

void QueryMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mQuery >> mType >> mFlags >> mMax
                 >> mMinLine >> mMaxLine >> mBuildIndex >> mPathFilters >> mCurrentFile
                 >> mUnsavedFiles;
}

Flags<Location::KeyFlag> QueryMessage::keyFlags(Flags<Flag> queryFlags)
{
    Flags<Location::KeyFlag> ret;
    if (!(queryFlags & NoContext))
        ret |= Location::ShowContext;
    if (queryFlags & NoColor)
        ret |= Location::NoColor;
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
    } else if (string == "elisp-list") {
        return ElispList;
    } else if (string == "imenu") {
        return IMenu;
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
    } else if (string == "symbol-info-exclude-targets") {
        return SymbolInfoExcludeTargets;
    } else if (string == "symbol-info-exclude-references") {
        return SymbolInfoExcludeReferences;
    } else if (string == "declaration-only") {
        return DeclarationOnly;
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
    } else if (string == "all-targets") {
        return AllTargets;
    }
    return NoFlag;
}
