#ifndef Token_h
#define Token_h

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

#include <rct/Map.h>

struct Token
{
    Token(const char *bytes = 0, int size = 0)
        : data(bytes), length(size)
    {}

    inline bool operator==(const Token &other) const
    {
        return length == other.length && !strncmp(data, other.data, length);
    }
    inline bool operator<(const Token &other) const
    {
        if (!data)
            return !other.data ? 0 : -1;
        if (!other.data)
            return 1;
        const int minLength = std::min(length, other.length);
        int ret = memcmp(data, other.data, minLength);
        if (!ret) {
            if (length < other.length) {
                ret = -1;
            } else if (other.length < length) {
                ret = 1;
            }
        }
        return ret;
    }

    const char *data;
    int length;

    static inline Map<Token, int> tokenize(const char *data, int size)
    {
        Map<Token, int> tokens;
        int tokenEnd = -1;
        for (int i=size - 1; i>=0; --i) {
            if (RTags::isSymbol(data[i])) {
                if (tokenEnd == -1)
                    tokenEnd = i;
            } else if (tokenEnd != -1) {
                addToken(data, i + 1, tokenEnd - i, tokens);
                tokenEnd = -1;
            }
        }
        if (tokenEnd != -1)
            addToken(data, 0, tokenEnd + 1, tokens);
        return tokens;
    }
private:
    static inline void addToken(const char *data, int pos, int len, Map<Token, int> &tokens)
    {
        int &val = tokens[Token(data + pos, len)];
        if (!val)
            val = pos;
    }


};


#endif
