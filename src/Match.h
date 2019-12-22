
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

#ifndef Match_h
#define Match_h

#include <regex>

#include "Location.h"
#include "rct/Flags.h"
#include "rct/Log.h"
#include "rct/Rct.h"
#include "rct/String.h"

class Match
{
public:
    enum Flag {
        Flag_None = 0x0,
        Flag_StringMatch = 0x1,
        Flag_Regex = 0x2,
        Flag_CaseInsensitive = 0x4
    };

    inline Match()
    {}

    inline Match(const String &pattern, Flags<Flag> f = Flag_StringMatch);
    inline Match(Match &&other) noexcept
        : mRegex(std::move(other.mRegex)), mPattern(std::move(other.mPattern)), mFlags(other.mFlags)
    {}
    inline Match(const Match &other)
        : mRegex(other.mRegex), mPattern(other.mPattern), mFlags(other.mFlags)
    {}

    Flags<Flag> flags() const { return mFlags; }

    inline bool match(const String &text) const
    {
        if (indexIn(text) != String::npos)
            return true;
        if (mFlags & Flag_StringMatch)
            return mPattern.indexOf(text, 0, (mFlags & Flag_CaseInsensitive
                                              ? String::CaseInsensitive
                                              : String::CaseSensitive)) != String::npos;
        return false;
    }

    inline size_t indexIn(const String &text) const
    {
        size_t index = String::npos;
        if (mFlags & Flag_StringMatch)
            index = text.indexOf(mPattern, 0, mFlags & Flag_CaseInsensitive ? String::CaseInsensitive : String::CaseSensitive);
        if (index == String::npos && mFlags & Flag_Regex) {
            index = Rct::indexIn(text, mRegex);
        }
        return index;
    }
    inline bool isEmpty() const
    {
        return !mFlags || mPattern.isEmpty();
    }

    inline std::regex regex() const
    {
        return mRegex;
    }

    inline String pattern() const
    {
        return mPattern;
    }
    uint32_t fileId() const
    {
        return Location::fileId(mPattern);
    }
private:
    std::regex mRegex;
    String mPattern;
    Flags<Flag> mFlags;
};
RCT_FLAGS(Match::Flag);

inline Log operator<<(Log log, const Match &match)
{
    String ret = "Match(flags: ";
    ret += String::number(match.flags().cast<unsigned int>(), 16);
    if (match.flags() & Match::Flag_Regex)
        ret += " rx";
    if (!match.pattern().isEmpty())
        ret += " pattern: " + match.pattern();
    ret += ")";
    log << ret;
    return log;
}

inline Match::Match(const String &pattern, Flags<Flag> f)
    : mFlags(f)
{
    if (mFlags & Flag_Regex) {
        try {
            if (mFlags & Flag_CaseInsensitive) {
                mRegex.assign(pattern.ref(), std::regex::icase);
            } else {
                mRegex.assign(pattern.ref());
            }
        } catch (const std::regex_error &err) {
            mFlags &= ~Flag_Regex;
        }
    }
    mPattern = pattern;
}




#endif
