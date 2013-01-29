#ifndef Match_h
#define Match_h

#include "ByteArray.h"
#include "RegExp.h"
#include "Log.h"

class Match
{
public:
    enum Flag {
        Flag_None = 0x0,
        Flag_StringMatch = 0x1,
        Flag_RegExp = 0x2,
        Flag_CaseInsensitive = 0x4
    };

    inline Match(const ByteArray &pattern = ByteArray(), unsigned flags = Flag_StringMatch)
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

    inline bool match(const ByteArray &text) const
    {
        if (indexIn(text) != -1)
            return true;
        if (mFlags & Flag_StringMatch)
            return mPattern.indexOf(text, 0, mFlags & Flag_CaseInsensitive ? ByteArray::CaseInsensitive : ByteArray::CaseSensitive) != -1;
        return false;
    }

    inline int indexIn(const ByteArray &text) const
    {
        int index = -1;
        if (mFlags & Flag_StringMatch)
            index = text.indexOf(mPattern, 0, mFlags & Flag_CaseInsensitive ? ByteArray::CaseInsensitive : ByteArray::CaseSensitive);
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

    inline ByteArray pattern() const
    {
        return mPattern;
    }
private:
    RegExp mRegExp;
    ByteArray mPattern;
    unsigned mFlags;
};

inline Log operator<<(Log log, const Match &match)
{
    ByteArray ret = "Match(flags: ";
    ret += ByteArray::number(match.flags(), 16);
    if (match.regExp().isValid())
        ret += " rx: " + match.regExp().pattern();
    if (!match.pattern().isEmpty())
        ret += " pattern: " + match.pattern();
    ret += ")";
    log << ret;
    return log;
}



#endif
