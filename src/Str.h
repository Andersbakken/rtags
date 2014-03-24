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

#ifndef Str_h
#define Str_h

#include <clang-c/Index.h>
#include <string.h>
#include <memory>

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
    bool operator>(const Str &other) const { return strcmp(data(), other.data()) > 0; }
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
    std::shared_ptr<Shared> mData;
};

#endif
