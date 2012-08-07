#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include <clang-c/Index.h>
#include <string.h>
#include <stdlib.h>
#include <ByteArray.h>
#include <Timer.h>
#include <Log.h>
#include <clang/Lex/Lexer.h>
#include <clang/Basic/SourceLocation.h>
#include <stack>

struct Entry
{
    Entry()
        : offset(-1), reference(false)
    {}

    std::string name, scope;
    int offset;
    bool reference;

};

enum { MacroIDBit = 1U << 31 };

class Parser
{
public:
    Parser(const char *file)
        : mFileName(file), mBraceCount(0)
    {
        FILE *f = fopen(mFileName, "r");
        assert(f);
        fseek(f, 0, SEEK_END);
        mSize = ftell(f);
        fseek(f, 0, SEEK_SET);
        mBuf = new char[mSize + 1];
        const int ret = fread(mBuf, sizeof(char), mSize, f);
        fclose(f);
        assert(ret == mSize);
        mBuf[ret] = 0;
        clang::SourceLocation loc;
        clang::LangOptions options;
        options.CPlusPlus = true;
        options.CPlusPlus0x = true;
        mLexer = new clang::Lexer(loc, options, mBuf, mBuf, mBuf + ret);
    }

    ~Parser()
    {
        delete mLexer;
        delete[] mBuf;
    }

    bool nextEntry(Entry *entry)
    {
        clang::Token token;
        int tokenLength;
        const char *tokenSpelling;
        while (true) {
            if (mLexer->LexFromRawLexer(token))
                break;
            switch (token.getKind()) {
            case clang::tok::l_paren:
            case clang::tok::r_paren:
            case clang::tok::l_brace:
            case clang::tok::r_brace:
            case clang::tok::semi:
            case clang::tok::period:
            case clang::tok::arrow:
                break;
            case clang::tok::raw_identifier:
                switch (mState.top()) {
                case Global:
                case FunctionBody: {
                    getTokenSpelling(token, tokenSpelling, tokenLength);
                    bool contextScope = false;
                    switch (tokenLength) {
                    case 5:
                        contextScope = !strncmp(tokenSpelling, "class", 5);
                        break;
                    case 6:
                        contextScope = !strncmp(tokenSpelling, "struct", 6);
                        break;
                    case 9:
                        contextScope = !strncmp(tokenSpelling, "namespace", 9);
                        break;
                    }
                    if (contextScope) {
                        mState.push(ContextPending);
                    }
                    break; }
                default:
                    break;
                }
                break; }
        }
        return false;
    }
private:
    void getTokenSpelling(const clang::Token &token, const char *&string, int &length)
    {
        string = mBuf + (token.getLocation().getRawEncoding() & ~MacroIDBit);
        length = token.getLength();
    }

    inline bool compareToken(const clang::Token &token, const char *string)
    {
        return !strncmp(mBuf + (token.getLocation().getRawEncoding() & ~MacroIDBit), string, token.getLength());
    }
    const char *mFileName;
    clang::Lexer *mLexer;
    int mBraceCount;
    char *mBuf;
    int mSize;
    enum State {
        Global,
        FunctionBody,
        ContextPending
    };
    std::stack<State> mState;
    std::vector<std::string> mContextScope;
};

// static inline void string(const clang::Token &token, const char *buf, std::string &str)
// {
//     str.assign(buf + (token.getLocation().getRawEncoding() & ~MacroIDBit), token.getLength());
// }

// static inline bool compare(const clang::Token &token, const char *&bu

int main(int argc, char **argv)
{
    Parser parser(argc > 1 ? argv[1] : "test.cpp");

    Entry entry;
    while (parser.nextEntry(&entry)) {
        printf("%d: %s (%s)%s\n", entry.offset, entry.name.c_str(), entry.scope.c_str(), entry.reference ? " reference" : "");
    }


    return 0;
}
