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

class Parser
{
public:
    enum Option {
        None = 0x0,
        CPlusPlus = 0x1
    };
    Parser(const char *file, unsigned opts)
        : mFileName(file), mBraceCount(0), mCurrentToken(0), mEntries(0)
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
        case tok::less:
            direction = 1;
            close = tok::greater;
            open = tok::less;
            break;
        case tok::greater:
            direction = -1;
            close = tok::less;
            open = tok::greater;
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
            i += direction;
        }
        return -1;
    }

    void parse(List<Entry> &entries)
    {
        const int verbosity = getenv("VERBOSE") ? atoi(getenv("VERBOSE")) : 0;
        const char *names[] = { "Global", "FunctionBody", "ContainerPending",
                                "FunctionPending", "Container" };

        mTokens.clear();
        Token token;
        while (!mLexer->LexFromRawLexer(token)) {
            if (verbosity)
                printf("%d %s \"%s\"\n", tokenOffset(token), token.getName(), tokenSpelling(token).constData());
            switch (token.getKind()) {
            case tok::l_brace:
            case tok::r_brace:
            case tok::l_paren:
            case tok::r_paren:
            case tok::semi:
            case tok::raw_identifier:
            case tok::coloncolon:
            case tok::colon:
            case tok::tilde:
            case tok::less:
            case tok::comma:
            case tok::greater: // without this one c++ style casts look like real function calls to us.
            case tok::equal: // without this one variable assignments look like real function calls to us.
                mTokens.append(token);
                break;
            default:
                break;
            }
        }

        entries.clear();
        mEntries = &entries;
        mState.push(State(Global));
        const int size = mTokens.size();
        while (mCurrentToken < size) {
            switch (kind(mCurrentToken)) {
            case tok::raw_identifier:
                handleRawIdentifier();
                break;
            case tok::l_brace:
                handleLeftBrace();
                break;
            case tok::colon:
                handleColon();
                break;
            case tok::r_brace:
                handleRightBrace();
                break;
            case tok::l_paren:
                handleLeftParen();
                break;
            case tok::semi:
                handleSemi();
                break;
            }
            ++mCurrentToken;
        }
    }
private:
    inline void handleRawIdentifier()
    {
        switch (mState.top().type) {
        case Global:
        case Container:
        case FunctionBody:
            switch (tokenKind(mCurrentToken - 1)) {
            case tok::comma:
            case tok::less:
                break;
            default: {
                const char *tokenSpl;
                int tokenLength;
                tokenSpelling(mTokens.at(mCurrentToken), tokenSpl, tokenLength);
                bool contextScope = false;
                switch (tokenLength) {
                case 5: contextScope = !strncmp(tokenSpl, "class", 5) || !strncmp(tokenSpl, "union", 5); break;
                case 6: contextScope = !strncmp(tokenSpl, "struct", 6); break;
                case 9: contextScope = !strncmp(tokenSpl, "namespace", 9); break;
                }
                if (contextScope) {
                    mState.push(State(ContainerPending));
                }
                break; }
            }
            break;
        default:
            break;
        }
    }
    inline void handleLeftBrace()
    {
        switch (mState.top().type) {
        case ContainerPending: {
            assert(!mState.empty());
            int containerIndex = mState.top().pendingIndex;
            if (containerIndex == -1 && tokenKind(mCurrentToken - 1) == tok::raw_identifier)
                containerIndex = mCurrentToken - 1;
            mState.pop();
            if (containerIndex != -1) {
                const Token &token = mTokens[containerIndex];
                Entry entry;
                entry.offset = tokenOffset(token);
                entry.name = tokenSpelling(token);
                entry.scope = mContainerScope;
                mState.push(State(Container, mBraceCount, entry.name));
                mContainerScope.reserve(mContainerScope.size() + entry.name.size() + 2);
                if (!mContainerScope.isEmpty())
                    mContainerScope.append("::");
                mContainerScope.append(entry.name);
                mEntries->append(entry);
            }
            break; }
        case FunctionPending: {
            int function = mState.top().pendingIndex;
            assert(function != -1);
            Entry entry;
            const Token &token = mTokens.at(function);
            entry.offset = tokenOffset(token);
            entry.name = tokenSpelling(token);
            // printf("Got into function pending prev is %s cur is %s for %s %s,%d\n",
            //        tok::getTokenName(tokenKind(function - 1)),
            //        tok::getTokenName(tokenKind(function)),
            //        entry.name.constData(), mFileName, entry.offset);

            if (tokenKind(function - 1) == tok::tilde) {
                // printf("[%s] %s:%d: if (tokenKind(function - 1) == tok::tilde) { [after]\n", __func__, __FILE__, __LINE__);
                entry.name.prepend('~');
                --entry.offset;
                --function;
            }
            entry.scope = mContainerScope;
            addContext(function, entry.scope);
            mEntries->append(entry);
            mState.pop();
            mState.push(State(FunctionBody, mBraceCount));
            break; }
        }
        ++mBraceCount;
    }

    inline void handleColon()
    {
        if (mState.top().type == ContainerPending
            && mState.top().pendingIndex == -1
            && tokenKind(mCurrentToken - 1) == tok::raw_identifier) {
            mState.top().pendingIndex = mCurrentToken - 1;
        }
    }

    inline void handleLeftParen()
    {
        switch (mState.top().type) {
        case FunctionBody: {
            int idx = -1;
            switch (tokenKind(mCurrentToken - 1)) {
            case tok::raw_identifier:
                idx = mCurrentToken - 1;
                break;
            case tok::greater:
                idx = findMatching(mCurrentToken - 1);
                if (tokenKind(idx - 1) == tok::raw_identifier) {
                    --idx;
                } else {
                    idx = -1;
                }
                break;
            default:
                break;
            }
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
                    keyWord = !strncmp(tokenSpl, "for", 3) || !strncmp(tokenSpl, "new", 3);
                    break;
                case 4:
                    keyWord = !strncmp(tokenSpl, "void", 4) || !strncmp(tokenSpl, "case", 4);
                    break;
                case 5:
                    keyWord = !strncmp(tokenSpl, "while", 5);
                    break;
                case 6:
                    keyWord = (!strncmp(tokenSpl, "switch", 6) || !strncmp(tokenSpl, "return", 6)
                               || !strncmp(tokenSpl,  "delete", 6));
                    break;
                case 8:
                    keyWord = !strncmp(tokenSpl, "operator", 8);
                    break;
                    // we could add sizeof and typeid here but it's kinda neat to find references to them
                }

                if (!keyWord) {
                    Entry entry;
                    entry.offset = tokenOffset(token);
                    entry.name = tokenSpelling(token);
                    entry.reference = true;
                    mEntries->append(entry);
                }
            }
            break; }
        case Container:
        case Global:
            if (tokenKind(mCurrentToken - 1) == tok::raw_identifier) {
                State state(FunctionPending);
                state.pendingIndex = mCurrentToken - 1;
                mState.push(state);
            }
            break;
        default:
            break;
        }
    }
    inline void handleRightBrace()
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
    inline void handleSemi()
    {
        switch (mState.top().type) {
        case ContainerPending: // forward declaration
            mState.pop();
            assert(!mState.empty());
            break;
        case FunctionPending:
            switch (tokenKind(mCurrentToken - 1)) {
            case tok::equal:
            case tok::r_paren: {
                const int pending = mState.top().pendingIndex;
                assert(pending != -1);
                Entry entry;
                entry.scope = mContainerScope;
                const Token &token = mTokens.at(pending);
                entry.offset = tokenOffset(token);
                entry.name = tokenSpelling(token);
                if (tokenKind(pending - 1) == tok::tilde) {
                    --entry.offset;
                    entry.name.prepend('~');
                }
                mEntries->append(entry);
                break; }
            default:
                break;
            }
            mState.pop();
            break;
        }
    }
    inline void addContext(int idx, ByteArray &ctx) const
    {
        const int old = idx;
        // printf("Calling addContext cur %s -1 %s -2  %s\n",
        //        tok::getTokenName(tokenKind(idx)),
        //        tok::getTokenName(tokenKind(idx - 1)),
        //        tok::getTokenName(tokenKind(idx - 2)));
        while (idx >= 2 && tokenKind(idx - 1) == tok::coloncolon && tokenKind(idx - 2) == tok::raw_identifier) {
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

    inline tok::TokenKind tokenKind(int idx) const // index <= 0 means from end
    {
        if (idx <= 0) {
            return mTokens.at(mTokens.size() - 1 + idx).getKind();
        } else {
            return mTokens.at(idx).getKind();
        }
    }

    inline void tokenSpelling(const Token &token, const char *&string, int &length) const
    {
        string = mBuf + tokenOffset(token);
        length = token.getLength();
    }

    inline ByteArray tokenSpelling(const Token &token) const
    {
        return ByteArray(mBuf + tokenOffset(token), token.getLength());
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
            : type(t), braceIndex(idx), name(n), pendingIndex(-1)
        {}
        StateType type;
        int braceIndex; // what brace index this state should get popped on or -1
        ByteArray name; // for classes/structs/namespaces
        int pendingIndex; // index of where the real class is for pending container states or where the real function is for pendingfunction
    };
    std::stack<State> mState;
    ByteArray mContainerScope;
    int mCurrentToken;
    List<Token> mTokens;
    List<Entry> *mEntries;
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
