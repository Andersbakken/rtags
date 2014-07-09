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

#ifndef Match_h
#define Match_h

#include <rct/String.h>
#include <rct/RegExp.h>
#include <rct/Log.h>

class Match
{
public:
    enum Flag {
        Flag_None = 0x0,
        Flag_StringMatch = 0x1,
        Flag_RegExp = 0x2,
        Flag_CaseInsensitive = 0x4
    };

    inline Match()
        : mFlags(0)
    {}

    inline Match(const String &pattern, unsigned flags = Flag_StringMatch)
        : mFlags(flags)
    {
        if (flags & Flag_RegExp)
            mRegExp = pattern;
        mPattern = pattern;
    }

    unsigned flags() const { return mFlags; }

    inline Match(const RegExp &regExp)
        : mRegExp(regExp), mPattern(regExp.pattern()), mFlags(Flag_RegExp)
    {}

    inline bool match(const String &text) const
    {
        if (indexIn(text) != -1)
            return true;
        if (mFlags & Flag_StringMatch)
            return mPattern.indexOf(text, 0, mFlags & Flag_CaseInsensitive ? String::CaseInsensitive : String::CaseSensitive) != -1;
        return false;
    }

    inline int indexIn(const String &text) const
    {
        int index = -1;
        if (mFlags & Flag_StringMatch)
            index = text.indexOf(mPattern, 0, mFlags & Flag_CaseInsensitive ? String::CaseInsensitive : String::CaseSensitive);
        if (index == -1 && mFlags & Flag_RegExp)
            index = mRegExp.indexIn(text);
        return index;
    }
    inline bool isEmpty() const
    {
        return !mFlags || mPattern.isEmpty();
    }

    inline RegExp regExp() const
    {
        return mRegExp;
    }

    inline String pattern() const
    {
        return mPattern;
    }
private:
    RegExp mRegExp;
    String mPattern;
    unsigned mFlags;
};

inline Log operator<<(Log log, const Match &match)
{
    String ret = "Match(flags: ";
    ret += String::number(match.flags(), 16);
    if (match.regExp().isValid())
        ret += " rx: " + match.regExp().pattern();
    if (!match.pattern().isEmpty())
        ret += " pattern: " + match.pattern();
    ret += ")";
    log << ret;
    return log;
}



#endif
