#include "Lexer.h"
#include <stdio.h>
#include <rct/Log.h>
#include <rct/Path.h>
#include <rct/String.h>
#include <rct/Rct.h>

struct Scope {
    Token::Range range;
    enum ScopeType {
        Function,
        Lambda,
        Namespace,
        Class,
        Struct,
        Union,
        ForLoop,
        WhileLoop,
        DoWhileLoop,
        If,
        Enum,
        Other
    } type;
    static const char *typeToString(ScopeType type)
    {
        switch (type) {
        case Function: return "Function";
        case Lambda: return "Lambda";
        case Namespace: return "Namespace";
        case Class: return "Class";
        case Struct: return "Struct";
        case Union: return "Union";
        case Other: return "Other";
        case ForLoop: return "ForLoop";
        case WhileLoop: return "WhileLoop";
        case DoWhileLoop: return "DoWhileLoop";
        case If: return "If";
        case Enum: return "Enum";
        }
        return 0;
    }
};

void analyze(const SourceFile &sourceFile)
{
    List<Scope> scopes;
    for (auto it = sourceFile.tokens.begin(); it != sourceFile.tokens.end(); ++it) {
        const Token &token = *it;
        if (token == '{') {
            auto it2 = it + 1;
            int depth = 1;
            while (it2 != sourceFile.tokens.end()) {
                if (*it2 == '{') {
                    ++depth;
                } else if (*it2 == '}' && !--depth) {
                    break;
                }
                ++it2;
            }
            if (it2 == sourceFile.tokens.end()) {
                error() << "GOTTA DEAL WITH THIS";
                break;
            }
            Scope scope;
            scope.range = token.range;
            scope.range.length = it2->range.offset - token.range.offset + it2->range.length;
            int parens = 0;
            bool hadParens = false;
            it2 = it;
            do {
                --it2;
                const Token &tok = *it2;
                if (tok == ')') {
                    hadParens = true;
                    ++parens;
                } else if (tok == '(') {
                    --parens;
                } else if (!parens) {
                    bool lambda = false;
                    if (it2->type == Token::Type_Symbol) {
                        auto it3 = it2;
                        --it3;
                        while (it3 != sourceFile.tokens.begin()) {
                            if (*it3 == "->") {
                                it2 = it3;
                                lambda = true;
                                break;
                            } else if (*it3 != "::" && it3->type != Token::Type_Symbol) {
                                break;
                            } else {
                                --it3;
                            }
                        }
                    }
                    if (!lambda)
                        break;
                }
            } while (it2 != sourceFile.tokens.begin());

            if (!hadParens) {
                scope.type = Scope::Other;
                if (it2 != sourceFile.tokens.begin() && it2->type == Token::Type_Symbol) {
                    --it2;
                    if (it2->type == Token::Type_Keyword) {
                        if (*it2 == "class") {
                            scope.type = Scope::Class;
                        } else if (*it2 == "struct") {
                            scope.type = Scope::Struct;
                        } else if (*it2 == "enum") {
                            scope.type = Scope::Enum;
                        } else if (*it2 == "namespace") {
                            scope.type = Scope::Namespace;
                        } else if (*it2 == "union") {
                            scope.type = Scope::Union;
                        }
                    }
                }
            } else if (*it2 == ']') {
                scope.type = Scope::Lambda;
            } else {
                scope.type = Scope::Function;
                if (it2->type == Token::Type_Keyword) {
                    if (*it2 == "if") {
                        scope.type = Scope::If;
                    } else if (*it2 == "for") {
                        scope.type = Scope::ForLoop;
                    } else if (*it2 == "while") {
                        scope.type = Scope::WhileLoop;
                    } else if (*it2 == "do") {
                        scope.type = Scope::DoWhileLoop;
                    }
                }
            }
            error() << "GOT A SCOPE AT" << Scope::typeToString(scope.type) << scope.range.toString() << scope.range.context();
        }
    }
}

int main(int argc, char **argv)
{
    for (int i=1; i<argc; ++i) {
        const Path p = argv[i];
        if (!p.isFile()) {
            fprintf(stderr, "Can't open %s for reading\n", argv[i]);;
            return 1;
        }
        const SourceFile sourceFile = SourceFile::create(p);
        for (const Token &token : sourceFile.tokens) {
            error() << token.toString() << token.context();
        }

        analyze(sourceFile);
    }

    return 0;
}
