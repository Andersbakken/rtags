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
#include <Timer.h>

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
    enum Option {
        None = 0x0,
        CPlusPlus = 0x1
    };
    Parser(const char *file, unsigned opts)
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
        if (ret != mSize) {
            printf("Read error %d %d %d %s - %s\n", ret, mSize, errno, strerror(errno), file);
        }
        assert(ret == mSize);
        mBuf[ret] = 0;
        SourceLocation loc;
        LangOptions options;
        if (opts & CPlusPlus) {
            options.CPlusPlus = true;
            options.CPlusPlus0x = true;
        }
        mLexer = new Lexer(loc, options, mBuf, mBuf, mBuf + ret);
    }

    ~Parser()
    {
        delete mLexer;
        delete[] mBuf;
    }
    // inline bool forwardTo(tok::TokenKind kind) const
    // {
    //     const int s = mTokens.size();
    //     while (mCurrentToken < s && mTokens.at(mCurrentToken).getKind() != kind)
    //         ++mCurrentToken;
    //     return mCurrentToken < s;
    // }

    // inline bool backwardTo(tok::TokenKind kind) const
    // {
    //     while (mCurrentToken >= 0 && mTokens.at(mCurrentToken).getKind() != kind)
    //         --mCurrentToken;
    //     return mCurrentToken >= 0;
    // }

    inline tok::TokenKind kind(int idx) const
    {
        if (idx < 0 || idx >= mTokens.size())
            return tok::NUM_TOKENS;
        return mTokens.at(idx).getKind();
    }

    inline int findMatching(int idx) const
    {
        tok::TokenKind close, open;
        int direction;
        switch (kind(idx)) {
        case tok::l_paren:
            direction = 1;
            close = tok::r_paren;
            open = tok::l_paren;
            break;
        case tok::r_paren:
            direction = -1;
            close = tok::l_paren;
            open = tok::r_paren;
            break;
        case tok::l_brace:
            direction = 1;
            close = tok::r_brace;
            open = tok::l_brace;
            break;
        case tok::r_brace:
            direction = -1;
            close = tok::l_brace;
            open = tok::r_brace;
            break;
        default:
            assert(0);
            return -1;
        }

        int count = 1;
        int i = idx + direction;
        while (true) {
            const tok::TokenKind k = kind(i);
            if (k == open) {
                ++count;
            } else if (k == close) {
                if (!--count)
                    return i;
            } else if (k == tok::NUM_TOKENS) {
                break;
            }
        }
        return -1;
    }

    void parse(List<Entry> &entries)
    {
        const int verbosity = atoi(getenv("VERBOSE"));
        const char *names[] = { "Global", "FunctionBody", "ContainerPending",
                                "FunctionPending", "Container" };

        mTokens.clear();
        while (!mLexer->LexFromRawLexer(token)) {
            if (verbosity)
                printf("%d %s \"%s\"\n", tokenOffset(token), token.getName(), tokenSpelling(token).constData());
            mTokens.append(token);
        }

        entries.clear();
        mEntries = &entries;
        mState.push(State(Global));
        const int size = mTokens.size();
        for (int i=0; i<size; ++i) {
            switch (kind(i)) {
            case tok::l_brace:
                handleLeftBrace(i, entries);
                break;
            case tok::colon:
                switch (mState.top().type) {
                case ContainerPending:
                    if (mState.top().pendingContainerIndex == -1)
                        mState.top().pendingContainerIndex = findLastToken(tok::raw_identifier, -1);
                    break;
                }
                break;
            case tok::r_brace:
                handleRightBrace(i);
                break;
            case tok::l_paren:
                handleLeftParen(i, entries);
                targetKind = tok::r_paren;
                break;
            case tok::semi:
                handleSemi(i, entries);
                break;
            case tok::raw_identifier:
                handleRawIdentifier(token);
                break;
            }
        }
    }
private:
    inline void handleLeftBrace(int idx, List<Entry> &entries)
    {
        switch (mState.top().type) {
        case ContainerPending:
            assert(!mState.empty());
            idx = (mState.top().pendingContainerIndex != -1
                   ? mState.top().pendingContainerIndex
                   : findLastToken(tok::raw_identifier, -1));
            mState.pop();
            if (idx != -1) {
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
                case 6:
                    keyWord = !strncmp(tokenSpl, "switch", 5);
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
            : type(t), braceIndex(idx), name(n), pendingContainerIndex(-1)
        {}
        StateType type;
        int braceIndex; // what brace index this state should get popped on or -1
        ByteArray name; // for classes/structs/namespaces
        int pendingContainerIndex; // index of where the real class is for pending container states
    };
    std::stack<State> mState;
    ByteArray mContainerScope;
    List<Token> mTokens;
};

static Path::VisitResult visit(const Path &path, void *userData)
{
    if (path.isFile()) {
        const char *ext = path.extension();
        if (ext) {
            const int len = strlen(ext);
            if (Path::isSource(ext, len) || Path::isHeader(ext, len)) {
                Parser parser(path.constData(), Parser::CPlusPlus);
                List<Entry> entries;
                parser.parse(entries);
                ++*reinterpret_cast<int*>(userData);
                // printf("%d %s => %d entries\n", *reinterpret_cast<int*>(userData),
                //        path.constData(), entries.size());
            }
        }
    }
    return Path::Recurse;
}

int main(int argc, char **argv)
{
    Timer timer;
    Path path(argc > 1 ? argv[1] : "test.cpp");
    path.resolve();
    if (path.isDir()) {
        int count = 0;
        path.visit(::visit, &count);
        printf("%d files in %dms\n", count, timer.elapsed());
    } else {
        Parser parser(path.constData(), Parser::CPlusPlus);

        List<Entry> entries;
        parser.parse(entries);
        for (List<Entry>::const_iterator it = entries.begin(); it != entries.end(); ++it) {
            const Entry &e = *it;
            printf("%s%s%s %s,%d%s\n", e.scope.nullTerminated(), e.scope.isEmpty() ? "" : "::",
                   e.name.constData(), path.nullTerminated(), e.offset, e.reference ? " reference" : "");
        }
    }


    return 0;
}
