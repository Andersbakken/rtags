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

#ifndef RLex_h
#define RLex_h

#include <rct/String.h>
#include <rct/Path.h>
#include <rct/Rct.h>
#include <stdint.h>

struct Token
{
    enum Type {
        Type_String,
        Type_Char,
        Type_Number,
        Type_Keyword,
        Type_Symbol,
        Type_Operator,
        Type_PreprocessingDirective,
        Type_Comment
    } type;
    struct Range {
        size_t offset;
        const char *ch;
        size_t length;

        String spelling() const { return String(ch, length); }
        String context() const
        {
            const char *start = ch;
            while (*start && *start != '\n')
                --start;
            if (*start)
                ++start;

            const char *end = ch;
            while (*end && *end != '\n')
                ++end;
            if (*end)
                --end;
            const String ret(start, end - start + 1);
            return Rct::colorize(ret, Rct::AnsiColor_BrightYellow, ch - start, std::min<size_t>(length, end - ch + 1));
        }
        String toString() const
        {
            return String::format("Offset: %zu Length: %zu", offset, length);
        }
    } range;
    String spelling() const { return range.spelling(); }
    String context() const { return range.context(); }
    String toString() const
    {
        return String::format("Type: %s %s Spelling: %s",
                              typeToString(type), range.toString().constData(), spelling().constData());

    }
    bool operator==(char other) const
    {
        return range.length == 1 && *range.ch == other;
    }

    bool operator==(const char *other) const
    {
        const size_t len = strlen(other);
        return len == range.length && !strncmp(range.ch, other, len);
    }

    bool operator!=(char other) const
    {
        return range.length != 1 || *range.ch != other;
    }

    bool operator!=(const char *other) const
    {
        const size_t len = strlen(other);
        return len != range.length || strncmp(range.ch, other, len);
    }

    static const char *typeToString(Type type)
    {
        switch (type) {
        case Type_String: return "String";
        case Type_Char: return "Char";
        case Type_Number: return "Number";
        case Type_Keyword: return "Keyword";
        case Type_Symbol: return "Symbol";
        case Type_Operator: return "Operator";
        case Type_PreprocessingDirective: return "Preprocessing Directive";
        case Type_Comment: return "Comment";
        }
        return 0;
    }
};

struct SourceFile
{
    static SourceFile create(const Path &path, bool *ok = 0);
    Path sourceFile;
    String code; // first byte is \0 so we can search backwards
    List<Token> tokens;
};

#endif
