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
    case tok::colon:
    case tok::tilde:
        return true;
    default:
        return false;
    }
}

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
        int idx;
        mState.push(State(Global));
        tok::TokenKind targetKind = tok::NUM_TOKENS;
        while (true) {
            if (mLexer->LexFromRawLexer(token))
                break;
            if (targetKind != tok::NUM_TOKENS) {
                if (token.getKind() != targetKind)
                    continue;
                targetKind = tok::NUM_TOKENS;
            }
            mTokens.append(token);
            if (getenv("VERBOSE") && isInteresting(token.getKind())) {
                const char *names[] = { "Global", "FunctionBody", "ContainerPending",
                                        "FunctionPending", "Container" };
                printf("%d %s \"%s\" state: %s\n",
                       tokenOffset(token), token.getName(), tokenSpelling(token).constData(), names[mState.top().type]);
            }

            switch (token.getKind()) {
            case tok::l_brace:
                handleLeftBrace(token, entries);
                break;
            case tok::colon:
                if (mState.top().type == Container && tokenKind(-1) == tok::r_paren)
                    targetKind = tok::l_brace; // need to read past the initializer list
                break;
            case tok::r_brace:
                handleRightBrace(token);
                break;
            case tok::l_paren:
                handleLeftParen(token, entries);
                targetKind = tok::r_paren;
                break;
            case tok::semi:
                handleSemi(token, entries);
                break;
            case tok::raw_identifier:
                handleRawIdentifier(token);
                break;
            }
        }
    }
private:
    inline void handleLeftBrace(const Token &token, List<Entry> &entries)
    {
        int idx;
        switch (mState.top().type) {
        case ContainerPending:
            mState.pop();
            assert(!mState.empty());
            if ((idx = findLastToken(tok::raw_identifier, -1)) != -1) {
                const Token &token = mTokens[idx];
                Entry entry;
                entry.offset = tokenOffset(token);
                entry.name = tokenSpelling(token);
                entry.scope = mContainerScope;
                mState.push(State(Container, mBraceCount, entry.name));
                mContainerScope.reserve(mContainerScope.size() + entry.name.size() + 2);
                if (!mContainerScope.isEmpty())
                    mContainerScope.append("::");
                mContainerScope.append(entry.name);
                entries.append(entry);

            }
            break;
        case Global:
        case Container:
            if ((idx = findLastToken(tok::l_paren, -1)) != -1) {
                if ((idx = findLastToken(tok::raw_identifier, idx)) != -1) {
                    Entry entry;
                    entry.offset = tokenOffset(mTokens.at(idx));
                    entry.name = tokenSpelling(mTokens.at(idx));
                    if (idx > 0 && tokenKind(idx - 1) == tok::tilde)
                        entry.name.prepend('~');
                    entry.scope = mContainerScope;
                    addContext(idx, entry.scope);
                    entries.append(entry);
                    mState.push(State(FunctionBody, mBraceCount));
                }
            }
            break;
        }
        ++mBraceCount;
    }
    inline void handleLeftParen(const Token &token, List<Entry> &entries)
    {
        if (mState.top().type == FunctionBody) {
            const int idx = findLastToken(tok::raw_identifier, -1);
            if (idx != -1) {
                const Token &token = mTokens[idx];
                const char *tokenSpl;
                int tokenLength;
                tokenSpelling(token, tokenSpl, tokenLength);
                bool keyWord = false;
                switch (tokenLength) {
                case 2:
                    keyWord = !strncmp(tokenSpl, "if", 2) || !strncmp(tokenSpl, "do", 2);
                    break;
                case 3:
                    keyWord = !strncmp(tokenSpl, "for", 3);
                    break;
                case 5:
                    keyWord = !strncmp(tokenSpl, "while", 5);
                    break;
                }

                if (!keyWord) {
                    Entry entry;
                    entry.offset = tokenOffset(token);
                    entry.name = tokenSpelling(token);
                    entry.reference = true;
                    entries.append(entry);
                }
            }
        }
    }
    inline void handleRightBrace(const Token &token)
    {
        if (mBraceCount > 0)
            --mBraceCount;
        if (mState.top().braceIndex == mBraceCount) {
            const State &top = mState.top();
            if (top.type == Container) {
                assert(!top.name.isEmpty());
                if (mContainerScope.size() == top.name.size()) {
                    mContainerScope.clear();
                } else {
                    mContainerScope.truncate(mContainerScope.size() - (top.name.size() + 2));
                }
            }
            mState.pop();
            assert(!mState.empty());
        }
    }
    inline void handleSemi(const Token &tokem, List<Entry> &entries)
    {
        int idx;
        switch (mState.top().type) {
        case ContainerPending: // forward declaration
            mState.pop();
            assert(!mState.empty());
            break;
        case Global:
        case Container:
            if (!mContainerScope.isEmpty()
                && mTokens.at(mTokens.size() - 2).getKind() == tok::r_paren
                && (idx = findLastToken(tok::l_paren, -1)) != -1) {
                if ((idx = findLastToken(tok::raw_identifier, idx)) != -1) {
                    Entry entry;
                    entry.scope = mContainerScope;
                    entry.offset = tokenOffset(mTokens.at(idx));
                    entry.name = tokenSpelling(mTokens.at(idx));
                    if (idx > 0 && tokenKind(idx - 1) == tok::tilde)
                        entry.name.prepend('~');
                    entries.append(entry);
                }
            }
            break;
        }
    }
    inline void handleRawIdentifier(const Token &token)
    {
        switch (mState.top().type) {
        case Global:
        case Container:
        case FunctionBody: {
            const char *tokenSpl;
            int tokenLength;
            tokenSpelling(token, tokenSpl, tokenLength);
            bool contextScope = false;
            switch (tokenLength) {
            case 5: contextScope = !strncmp(tokenSpl, "class", 5); break;
            case 6: contextScope = !strncmp(tokenSpl, "struct", 6); break;
            case 9: contextScope = !strncmp(tokenSpl, "namespace", 9); break;
            }
            if (contextScope) {
                mState.push(State(ContainerPending));
            }
            break; }
        default:
            break;
        }
    }
    inline void addContext(int idx, ByteArray &ctx) const
    {
        const int old = idx;
        while (idx >= 2 && mTokens.at(idx - 1).getKind() == tok::coloncolon && mTokens.at(idx - 2).getKind() == tok::raw_identifier) {
            idx -= 2;
        }
        if (idx != old) {
            const int p = tokenOffset(mTokens.at(idx));
            if (ctx.isEmpty()) {
                ctx = ByteArray(mBuf + p, tokenOffset(mTokens.at(old - 1)) - p);
            } else {
                ctx.append("::");
                ctx += ByteArray(mBuf + p, tokenOffset(mTokens.at(old - 1)) - p);
            }
        }
    }
    inline int findLastToken(tok::TokenKind kind, int from) const
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
    inline void tokenSpelling(const Token &token, const char *&string, int &length) const
    {
        string = mBuf + tokenOffset(token);
        length = token.getLength();
    }

    inline tok::TokenKind tokenKind(int idx) const // index <= 0 means from end
    {
        if (idx <= 0) {
            return mTokens.at(mTokens.size() - 1 + idx).getKind();
        } else {
            return mTokens.at(idx).getKind();
        }
    }

    inline ByteArray tokenSpelling(const Token &token) const
    {
        return ByteArray(mBuf + tokenOffset(token), token.getLength());
    }

    inline bool compareToken(const Token &token, const char *string) const
    {
        return !strncmp(mBuf + tokenOffset(token), string, token.getLength());
    }
    const char *mFileName;
    Lexer *mLexer;
    int mBraceCount, mSize;
    char *mBuf;
    enum StateType {
        Global,
        FunctionBody,
        ContainerPending,
        FunctionPending,
        Container
    };
    struct State {
        State(StateType t = Global, int idx = -1, const ByteArray &n = ByteArray())
            : type(t), braceIndex(idx), name(n)
        {}
        StateType type;
        int braceIndex; // what brace index this state should get popped on or -1
        ByteArray name; // for classes/structs/namespaces
    };
    std::stack<State> mState;
    ByteArray mContainerScope;
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
