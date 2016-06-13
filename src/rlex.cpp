#include <stdio.h>
#include <rct/Log.h>
#include <rct/Path.h>
#include <rct/String.h>
#include <rct/Rct.h>

#if 1 /*& fobae */ && 2

#endif
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
    size_t offset;
    const char *ch;
    size_t length;
    String spelling() const { return String(ch, length); }
    String context(const char *document) const
    {
        const char *start = ch;
        while (start != document && *start != '\n')
            --start;
        if (start != document)
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
        return String::format("Type: %s Offset: %zu Length: %zu Spelling: %s",
                              typeToString(type), offset, length, spelling().constData());

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

int main(int argc, char **argv)
{
    for (int i=1; i<argc; ++i) {
        Path p = argv[i];
        if (!p.isFile()) {
            fprintf(stderr, "Can't open %s for reading\n", argv[i]);;
            return 1;
        }
        List<Token> tokens;
        String source = p.readAll();
        const char *ch = source.constData();
        const char *start = ch;
        const size_t length = source.size();
        auto eatSpace = [&ch]() {
            while (true) {
                if (!*ch)
                    return false;
                if (!isspace(*ch))
                    break;
                ++ch;
            }
            return true;
        };
        auto addToken = [&tokens, start, &ch](Token::Type type, const char *from) {
            tokens.emplace_back(Token { type, static_cast<size_t>(from - start), from, static_cast<size_t>(ch - from) });
        };

        auto addQuoted = [&addToken, &ch]() {
            const char *start = ch;
            int escape = 0;
            while (*++ch) {
                if (*ch == *start) {
                    if (escape % 2 == 0) {
                        ++ch;
                        addToken(*start == '\'' ? Token::Type_Char : Token::Type_String, start);
                        return true;
                    }
                } else if (*ch == '\\') {
                    ++escape;
                } else {
                    escape = 0;
                }
            }
            return false;
        };

        auto isKeyword = [&ch](const char *start) {
            const char *keyWords[] = {
                "alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit", "atomic_noexcept", "auto",
                "bitand", "bitor", "bool", "break", "case", "catch", "char", "char16_t", "char32_t", "class", "compl", "concept",
                "const", "constexpr", "const_cast", "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast",
                "else", "enum", "explicit", "export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int",
                "import", "long", "module", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or",
                "or_eq", "private", "protected", "public", "register", "reinterpret_cast", "requires", "return", "short", "signed",
                "sizeof", "static", "static_assert", "static_cast", "struct", "switch", "synchronized", "template", "this",
                "thread_local", "throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using", "virtual",
                "void", "volatile", "wchar_t", "while", "xor", "xor_eq"
            };
            for (const char *keyWord : keyWords) {
                if (!strncmp(start, keyWord, ch - start)) {
                    return true;
                }
            }
            return false;
        };

        auto processOperator = [&ch, &addToken]() {
            static const String operators[] = {
                "reinterpret_cast", "dynamic_cast", "static_cast", "sizeof", "<<=",
                ">>=", "<<", ">>", ">=", "<=", "&&", "||", "++", "--", "==", "!=", "+=", "-=", "*=", "/=", "%=", "&=", "^=", "|=", "::", "()",
                "(", ")", "+", "-", "*", "/", "%", ">", "<", "!", "&", "|", "^", "=", "?", ":", ",", ".", "{", "}", "[", "]", ";"
            };

            if (*ch == '~' && !isalpha(*ch + 1) && *(ch + 1) != '_') {
                ++ch;
                addToken(Token::Type_Operator, ch - 1);
                return true;
            }
            for (const String &str : operators) {
                if (!strncmp(ch, str.constData(), str.length())) {
                    ch += str.length();
                    addToken(Token::Type_Operator, ch - str.length());
                    return true;
                }
            }
            return false;
        };

        auto processComment = [&ch, &addToken, start, length]() {
            if (*ch == '/') {
                if (*(ch + 1) == '/') {
                    const char *last = ch++;
                    bool done;
                    do {
                        while (*ch != '\n') {
                            ++ch;
                        }
                        done = *(ch - 1) != '\\';
                    } while (!done);
                    addToken(Token::Type_Comment, last);
                    return true;
                } else if (*(ch + 1) == '*') {
                    const char *last = ch;
                    const char *end = strstr(ch, "*/");
                    if (end) {
                        ch = end + 2;
                    } else {
                        ch = start + length;
#warning gotta report error
                    }
                    addToken(Token::Type_Comment, last);
                    return true;
                }
            }

            return false;
        };

        while (ch && *ch) {
            if (!eatSpace())
                break;

            // printf("CONSIDERING [%s]\n", String(ch, 3).constData());

            if (isdigit(*ch) || *ch == '.') {
                const char *last = ch;
                do {
                    ++ch;
                } while (isdigit(*ch) || *ch == '.');
                addToken(Token::Type_Number, last);
            } else if (*ch == '"') {
                addQuoted();
            } else if (*ch == '\'') {
                addQuoted();
            } else if (*ch == '#') {
                const char *last = ch;
                do {
                    ++ch;
                } while (*ch && isalpha(*ch));
                bool done;
                do {
                    while (*ch != '\n') {
                        processComment();
                        ++ch;
                    }
                    done = *(ch - 1) != '\\';
                } while (!done);
                addToken(Token::Type_PreprocessingDirective, last);
            } else if (!processComment() && !processOperator() &&
                       (isalpha(*ch) || *ch == '_' || (*ch == '~' && isalpha(*(ch + 1))))) {
                const char *last = ch;
                while (isalnum(*ch) || *ch == '_')
                    ++ch;
                addToken(isKeyword(last) ? Token::Type_Keyword : Token::Type_Symbol, last);
            }
        }
        for (const Token &token : tokens) {
            error() << token.toString() << token.context(start);
        }

    }

    return 0;
}
