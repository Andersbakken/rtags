#ifndef RegExp_h
#define RegExp_h

#include <regex.h>
#include "String.h"
#include "Log.h"

class RegExp
{
public:
    RegExp(const String &pattern = String(), uint32_t flags = 0)
        : mPattern(pattern), mFlags(flags), mState(Unset)
    {}

    ~RegExp()
    {
        if (mState == Success) {
            regfree(&mRegex);
        }
    }

    bool isValid() const
    {
        if (mState == Unset) {
            if (mPattern.isEmpty()) {
                mState = Error;
            } else {
                mState = regcomp(&mRegex, mPattern.constData(), mFlags) ? Error : Success;
            }
        }
        return mState == Success;
    }

    bool isEmpty() const
    {
        return mPattern.isEmpty() || !isValid();
    }

    String pattern() const
    {
        return mPattern;
    }

    RegExp &operator=(const String &pattern)
    {
        clear();
        mPattern = pattern;
        return *this;
    }

    void clear()
    {
        mPattern.clear();
        mFlags = 0;
        mState = Unset;
    }

    struct Capture {
        Capture()
            : index(-1)
        {}
        int index;
        String capture;
    };

    int indexIn(const String &string, int offset = 0, List<Capture> *caps = 0, uint32_t flags = 0) const
    {
        if (!isValid())
            return -1;
        regmatch_t captures[10];
        if (regexec(&mRegex, string.constData() + offset, sizeof(captures) / sizeof(regmatch_t), captures, flags)) {
            return -1;
        }
        if (caps) {
            for (unsigned i=0; i<sizeof(captures) / sizeof(regmatch_t); ++i) {
                if (captures[i].rm_so != -1) {
                    Capture capture;
                    capture.index = captures[i].rm_so;
                    capture.capture = string.mid(capture.index, captures[i].rm_eo - capture.index);
                    caps->append(capture);
                } else {
                    break;
                }
            }
        }
        return captures[0].rm_so;
    }
private:
    String mPattern;
    uint32_t mFlags;
    mutable regex_t mRegex;
    enum State {
        Unset,
        Error,
        Success
    } mutable mState;
};

inline Log operator<<(Log stream, const RegExp &rx)
{
    stream << "RegExp(" << rx.pattern() << ")";
    return stream;
}

#endif
