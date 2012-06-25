#ifndef Str_h
#define Str_h

#include <clang-c/Index.h>
#include <tr1/memory>

class Str
{
public:
    Str()
    {}
    Str(CXString string)
    {
        if (clang_getCString(string)) {
            mData.reset(new Shared(string));
        } else {
            clang_disposeString(string);
        }
    }

    bool operator==(const Str &other) const { return !strcmp(data(), other.data()); }
    bool operator<(const Str &other) const { return strcmp(data(), other.data()) < 0; }
    bool operator>(const Str &other) const { return !strcmp(data(), other.data()) > 0; }
    const char *data() const { return mData ? clang_getCString(mData->string) : 0; }
    int length() const
    {
        if (mData) {
            if (mData->length == -1)
                mData->length = strlen(clang_getCString(mData->string));
            return mData->length;
        }
        return 0;
    }
private:
    class Shared
    {
    public:
        Shared(CXString str)
            : string(str), length(-1)
        {}
        ~Shared()
        {
            clang_disposeString(string);
        }
        CXString string;
        mutable int length;
    };
    std::tr1::shared_ptr<Shared> mData;
};

#endif
