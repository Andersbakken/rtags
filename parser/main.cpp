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
#include <ByteArray.h>

using namespace clang;
static inline int tokenOffset(const Token &token)
{
    enum { MacroIDBit = 1U << 31 };
    return token.getLocation().getRawEncoding() & ~MacroIDBit;
}

struct Entry
{
    Entry()
        : offset(-1), reference(false)
    {}

    ByteArray name, scope;
    int offset;
    bool reference;
};

static inline bool isInteresting(tok::TokenKind kind)
{
    switch (kind) {
    case tok::l_brace:
    case tok::r_brace:
    case tok::l_paren:
    case tok::r_paren:
    case tok::semi:
    case tok::raw_identifier:
    case tok::coloncolon:
        return true;
    default:
        return false;
    }
}

class Parser
{
public:
    Parser(const char *file)
        : mFileName(file), mBraceCount(0), mFunctionBraceCount(-1)
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
        SourceLocation loc;
        LangOptions options;
        options.CPlusPlus = true;
        options.CPlusPlus0x = true;
        mLexer = new Lexer(loc, options, mBuf, mBuf, mBuf + ret);
    }

    ~Parser()
    {
        delete mLexer;
        delete[] mBuf;
    }
    void parse(List<Entry> &entries)
    {
        entries.clear();
        Token token;
        int tokenLength, idx;
        const char *tokenSpl;
        mState.push(Global);
        while (true) {
            if (mLexer->LexFromRawLexer(token))
                break;
            mTokens.append(token);
            if (getenv("VERBOSE") && isInteresting(token.getKind())) {
                const char *names[] = { "global", "functionbody", "contextpending", "functionpending" };
                printf("%d %s \"%s\" state: %s\n",
                       tokenOffset(token), token.getName(), tokenSpelling(token).constData(), names[mState.top()]);
            }

            switch (token.getKind()) {
            case tok::l_brace:
                switch (mState.top()) {
                case ContextPending:
                    if ((idx = findLastToken(tok::raw_identifier, -1)) != -1) {
                        const Token &token = mTokens[idx];
                        Entry entry;
                        entry.offset = tokenOffset(token);
                        entry.name = tokenSpelling(token);
                        entry.scope = mScope;
                        // printf("Used a scope %s\n", mScope.nullTerminated());
                        mContextScope.push(std::make_pair(entry.name, mBraceCount));
                        mScope.reserve(mScope.size() + entry.name.size() + 2);
                        if (!mScope.isEmpty())
                            mScope.append("::");
                        mScope.append(entry.name);
                        // printf("Scope is now %s\n", mScope.nullTerminated());
                        // printf("Added one %s %s %d %d\n", entry.scope.nullTerminated(), entry.name.nullTerminated(),
                        // entry.offset, __LINE__);
                        entries.append(entry);

                    }
                    mState.pop();
                    break;
                case Global:
                    if ((idx = findLastToken(tok::l_paren, -1)) != -1) {
                        if ((idx = findLastToken(tok::raw_identifier, idx)) != -1) {
                            Entry entry;
                            entry.offset = tokenOffset(mTokens.at(idx));
                            entry.name = tokenSpelling(mTokens.at(idx));
                            entry.scope = mScope;
                            addContext(idx, entry.scope);
                            printf("used scope %s %s for %s\n", mScope.constData(), entry.scope.constData(), entry.name.constData());
                            entries.append(entry);
                            // printf("Added one %s %s %d %d\n", entry.scope.nullTerminated(), entry.name.nullTerminated(),
                            //        entry.offset, __LINE__);
                            mState.push(FunctionBody);
                            assert(mFunctionBraceCount == -1);
                            mFunctionBraceCount = mBraceCount;
                        }
                        // printf("Pushed to function %d\n", mFunctionBraceCount);
                    }
                    break;
                }
                ++mBraceCount;
                break;
            case tok::r_brace:
                --mBraceCount;
                // printf("Got rightbrace %d\n", mFunctionBraceCount);
                if (!mContextScope.empty() && mContextScope.top().second == mBraceCount) {
                    if (mContextScope.size() == 1) {
                        mScope.clear();
                    } else {
                        mScope.truncate(mScope.size() - (mContextScope.top().first.size() + 2));
                    }
                    mContextScope.pop();
                } else if (mBraceCount == mFunctionBraceCount) {
                    mFunctionBraceCount = -1;
                    assert(mState.top() == FunctionBody);
                    mState.pop();
                }
                break;
            case tok::l_paren:
                if (mState.top() == FunctionBody) {
                    const int idx = findLastToken(tok::raw_identifier, -1);
                    if (idx != -1) {
                        const Token &token = mTokens[idx];
                        Entry entry;
                        entry.offset = tokenOffset(token);
                        entry.name = tokenSpelling(token);
                        entry.reference = true;
                        entries.append(entry);
                        // printf("Added one %s %s %d %d\n", entry.scope.nullTerminated(), entry.name.nullTerminated(),
                        //        entry.offset, __LINE__);
                    }
                }
                break;
            case tok::r_paren:
                break;
            case tok::semi:
                switch (mState.top()) {
                case ContextPending:
                    mState.pop();
                    break;
                case Global:
                    // printf("Got some stuff here %d %d %d\n", (int)mContextScope.size(), findLastToken(tok::raw_identifier),
                    //        tokenOffset(token));
                    if (!mContextScope.empty() && (idx = findLastToken(tok::l_paren, -1)) != -1) {
                        if ((idx = findLastToken(tok::raw_identifier, idx)) != -1) {
                            Entry entry;
                            entry.scope = mScope;
                            // printf("Used a scope %s\n", mScope.nullTerminated());
                            entry.offset = tokenOffset(mTokens.at(idx));
                            entry.name = tokenSpelling(mTokens.at(idx));
                            entries.append(entry);
                            printf("Added one %s %s %d %d %d\n", entry.scope.nullTerminated(), entry.name.nullTerminated(),
                                   entry.offset, __LINE__, tokenOffset(token));
                        }
                    }
                    break;
                }
                break;
            case tok::period:
            case tok::arrow:
                break;
            case tok::raw_identifier:
                switch (mState.top()) {
                case Global:
                case FunctionBody: {
                    tokenSpelling(token, tokenSpl, tokenLength);
                    bool contextScope = false;
                    switch (tokenLength) {
                    case 5:
                        contextScope = !strncmp(tokenSpl, "class", 5);
                        break;
                    case 6:
                        contextScope = !strncmp(tokenSpl, "struct", 6);
                        break;
                    case 9:
                        contextScope = !strncmp(tokenSpl, "namespace", 9);
                        break;
                    }
                    if (contextScope) {
                        mState.push(ContextPending);
                    }
                    break; }
                default:
                    break;
                }
                break;
            }
        }
    }
private:
    void addContext(int idx, ByteArray &ctx) const
    {
        const int old = idx;
        while (idx >= 2 && mTokens.at(idx - 1).getKind() == tok::coloncolon && mTokens.at(idx - 2).getKind() == tok::raw_identifier) {
            idx -= 2;
        }
        if (idx != old) {
            const int p = tokenOffset(mTokens.at(idx));
            if (ctx.isEmpty()) {
                ctx = ByteArray(mBuf + p, tokenOffset(mTokens.at(old - 1)) - p);
                // printf("added some context here %s\n", ctx.constData());
            } else {
                // printf("addContext not empty %s\n", ctx.constData());
                ctx.append("::");
                ctx += ByteArray(mBuf + p, tokenOffset(mTokens.at(old - 1)) - p);
            }
        }
    }
    int findLastToken(tok::TokenKind kind, int from) const
    {
        if (from == -1)
            from = mTokens.size() - 1;
        while (from >= 0) {
            if (mTokens.at(from).is(kind)) {
                break;
            }
            --from;
        }
        return from;
    }
    void tokenSpelling(const Token &token, const char *&string, int &length) const
    {
        string = mBuf + tokenOffset(token);
        length = token.getLength();
    }

    ByteArray tokenSpelling(const Token &token) const
    {
        return ByteArray(mBuf + tokenOffset(token), token.getLength());
    }

    inline bool compareToken(const Token &token, const char *string) const
    {
        return !strncmp(mBuf + tokenOffset(token), string, token.getLength());
    }
    const char *mFileName;
    Lexer *mLexer;
    int mBraceCount, mSize, mFunctionBraceCount;
    char *mBuf;
    enum State {
        Global,
        FunctionBody,
        ContextPending,
        FunctionPending
    };
    std::stack<State> mState;
    std::stack<std::pair<ByteArray, int> > mContextScope;
    ByteArray mScope;
    List<Token> mTokens;
};

int main(int argc, char **argv)
{
    Parser parser(argc > 1 ? argv[1] : "test.cpp");

    List<Entry> entries;
    parser.parse(entries);
    for (List<Entry>::const_iterator it = entries.begin(); it != entries.end(); ++it) {
        const Entry &e = *it;
        printf("%s%s%s %d%s\n", e.scope.nullTerminated(), e.scope.isEmpty() ? "" : "::",
               e.name.constData(), e.offset, e.reference ? " reference" : "");
    }


    return 0;
}
