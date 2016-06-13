#include "Lexer.h"
#include <rct/Log.h>

SourceFile SourceFile::create(const Path &path, bool *ok)
{
    SourceFile ret;
    {
        FILE *f = fopen(path.constData(), "r");
        if (!f) {
            if (ok)
                *ok = false;
            return ret;
        }
        int fd = fileno(f);
        assert(fd != -1);
        struct stat st;
        if (!fstat(fd, &st)) {
            int size = static_cast<int>(st.st_size);
            if (size) {
                ret.code.resize(size + 1);
                ret.code[0] = '\0';
                const int read = fread(&ret.code[1], sizeof(char), size, f);
                fclose(f);
                if (read != size) {
                    if (ok)
                        *ok = false;
                    ret.code.clear();
                    return ret;
                }
            }
        }
    }
    ret.sourceFile = path;

    const char *ch = ret.code.constData() + 1;
    const char *start = ch;
    const size_t length = ret.code.size() - 1;
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
    auto addToken = [&ret, start, &ch](Token::Type type, const char *from) {
        ret.tokens.emplace_back(Token { type, Token::Range { static_cast<size_t>(from - start), from, static_cast<size_t>(ch - from) } });
    };

    auto addQuoted = [&addToken, &ch]() {
        const char *start = ch;
        int escapes = 0;
        while (*++ch) {
            if (*ch == *start) {
                if (escapes % 2 == 0) {
                    ++ch;
                    addToken(*start == '\'' ? Token::Type_Char : Token::Type_String, start);
                    return true;
                } else {
                    escapes = 0;
                }
            } else if (*ch == '\\') {
                ++escapes;
            } else {
                escapes = 0;
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
            ">>=", "<<", ">>", ">=", "<=", "&&", "||", "++", "--", "==", "!=", "+=", "-=", "*=", "/=", "%=", "&=", "^=", "|=", "::", "->",
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

#ifndef NDEBUG
    long last = INT_MAX;
#endif
    while (ch && *ch) {
        // printf("%zu/%zu [%c]\n", ch - start, length, *ch);
        assert(ch - start != last);
        // if (ch - start == last) {
        //     break;
        // }
#ifndef NDEBUG
        last = ch - start;
#endif
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
    return ret;
}
