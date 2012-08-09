#ifndef GRParser_h
#define GRParser_h

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
#include "ScopedDB.h"

class GRParser
{
public:
    enum Option {
        None = 0x0,
        CPlusPlus = 0x1
    };
    GRParser()
        : mLexer(0), mBraceCount(0), mSize(0), mCurrentToken(0), mBuf(0)
    {}


    ~GRParser()
    {
        delete mLexer;
        delete[] mBuf;
    }

    bool parse(ScopedDB db, const Path &file, unsigned opts)
    {
        mDB = db;
        mFileName = file;
        FILE *f = fopen(mFileName.constData(), "r");
        assert(f);
        fseek(f, 0, SEEK_END);
        mSize = ftell(f);
        fseek(f, 0, SEEK_SET);
        mBuf = new char[mSize + 1];
        const int ret = fread(mBuf, sizeof(char), mSize, f);
        fclose(f);
        if (ret != mSize) {
            printf("Read error %d %d %d %s - %s\n", ret, mSize, errno, strerror(errno), file.constData());
            return false;
        }
        assert(ret == mSize);
        mBuf[ret] = 0;
        clang::SourceLocation loc;
        clang::LangOptions options;
        if (opts & CPlusPlus) {
            options.CPlusPlus = true;
            options.CPlusPlus0x = true;
        }
        mLexer = new clang::Lexer(loc, options, mBuf, mBuf, mBuf + ret);
        const int verbosity = getenv("VERBOSE") ? atoi(getenv("VERBOSE")) : 0;
        mTokens.clear();
        clang::Token token;
        while (!mLexer->LexFromRawLexer(token)) {
            if (verbosity)
                printf("%d %s \"%s\"\n", tokenOffset(token), token.getName(), tokenSpelling(token).constData());
            switch (token.getKind()) {
            case clang::tok::l_brace:
            case clang::tok::r_brace:
            case clang::tok::l_paren:
            case clang::tok::r_paren:
            case clang::tok::semi:
            case clang::tok::raw_identifier:
            case clang::tok::coloncolon:
            case clang::tok::colon:
            case clang::tok::tilde:
            case clang::tok::less:
            case clang::tok::comma:
            case clang::tok::greater: // without this one c++ style casts look like real function calls to us.
            case clang::tok::equal: // without this one variable assignments look like real function calls to us.
                mTokens.append(token);
                break;
            default:
                break;
            }
        }

        mState.push(State(Global));
        const int size = mTokens.size();
        while (mCurrentToken < size) {
            switch (kind(mCurrentToken)) {
            case clang::tok::raw_identifier:
                handleRawIdentifier();
                break;
            case clang::tok::l_brace:
                handleLeftBrace();
                break;
            case clang::tok::colon:
                handleColon();
                break;
            case clang::tok::r_brace:
                handleRightBrace();
                break;
            case clang::tok::l_paren:
                handleLeftParen();
                break;
            case clang::tok::semi:
                handleSemi();
                break;
            default:
                break;
            }
            ++mCurrentToken;
        }

        return true;
    }

private:
    inline void addEntry(const ByteArray &name, const ByteArray &containerScope, int offset)
    {}
    inline void addReference(const ByteArray &name, int offset)
    {}
    inline clang::tok::TokenKind kind(int idx) const
    {
        if (idx < 0 || idx >= mTokens.size())
            return clang::tok::NUM_TOKENS;
        return mTokens.at(idx).getKind();
    }

    inline int findMatching(int idx) const
    {
        clang::tok::TokenKind close, open;
        int direction;
        switch (kind(idx)) {
        case clang::tok::l_paren:
            direction = 1;
            close = clang::tok::r_paren;
            open = clang::tok::l_paren;
            break;
        case clang::tok::r_paren:
            direction = -1;
            close = clang::tok::l_paren;
            open = clang::tok::r_paren;
            break;
        case clang::tok::l_brace:
            direction = 1;
            close = clang::tok::r_brace;
            open = clang::tok::l_brace;
            break;
        case clang::tok::r_brace:
            direction = -1;
            close = clang::tok::l_brace;
            open = clang::tok::r_brace;
            break;
        case clang::tok::less:
            direction = 1;
            close = clang::tok::greater;
            open = clang::tok::less;
            break;
        case clang::tok::greater:
            direction = -1;
            close = clang::tok::less;
            open = clang::tok::greater;
            break;
        default:
            assert(0);
            return -1;
        }

        int count = 1;
        int i = idx + direction;
        while (true) {
            const clang::tok::TokenKind k = kind(i);
            if (k == open) {
                ++count;
            } else if (k == close) {
                if (!--count)
                    return i;
            } else if (k == clang::tok::NUM_TOKENS) {
                break;
            }
            i += direction;
        }
        return -1;
    }
private:
    inline void handleRawIdentifier()
    {
        switch (mState.top().type) {
        case Global:
        case Container:
        case FunctionBody:
            switch (tokenKind(mCurrentToken - 1)) {
            case clang::tok::comma:
            case clang::tok::less:
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
            if (containerIndex == -1 && tokenKind(mCurrentToken - 1) == clang::tok::raw_identifier)
                containerIndex = mCurrentToken - 1;
            mState.pop();
            if (containerIndex != -1) {
                const clang::Token &token = mTokens[containerIndex];
                const ByteArray name = tokenSpelling(token);
                addEntry(tokenSpelling(token), mContainerScope, tokenOffset(token));
                // GREntry entry;
                // entry.offset = tokenOffset(token);
                // entry.name = tokenSpelling(token);
                // entry.scope = mContainerScope;
                mState.push(State(Container, mBraceCount, name));
                mContainerScope.reserve(mContainerScope.size() + name.size() + 2);
                if (!mContainerScope.isEmpty())
                    mContainerScope.append("::");
                mContainerScope.append(name);
            }
            break; }
        case FunctionPending: {
            int function = mState.top().pendingIndex;
            assert(function != -1);
            const clang::Token &token = mTokens.at(function);
            int offset = tokenOffset(token);
            ByteArray name = tokenSpelling(token);

            if (tokenKind(function - 1) == clang::tok::tilde) {
                name.prepend('~');
                --offset;
                --function;
            }
            ByteArray scope = mContainerScope;
            addContext(function, scope);
            addEntry(name, scope, offset);
            mState.pop();
            mState.push(State(FunctionBody, mBraceCount));
            break; }
        default:
            break;
        }
        ++mBraceCount;
    }

    inline void handleColon()
    {
        if (mState.top().type == ContainerPending
            && mState.top().pendingIndex == -1
            && tokenKind(mCurrentToken - 1) == clang::tok::raw_identifier) {
            mState.top().pendingIndex = mCurrentToken - 1;
        }
    }

    inline void handleLeftParen()
    {
        switch (mState.top().type) {
        case FunctionBody: {
            int idx = -1;
            switch (tokenKind(mCurrentToken - 1)) {
            case clang::tok::raw_identifier:
                idx = mCurrentToken - 1;
                break;
            case clang::tok::greater:
                idx = findMatching(mCurrentToken - 1);
                if (tokenKind(idx - 1) == clang::tok::raw_identifier) {
                    --idx;
                } else {
                    idx = -1;
                }
                break;
            default:
                break;
            }
            if (idx != -1) {
                const clang::Token &token = mTokens[idx];
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

                if (!keyWord)
                    addReference(tokenSpelling(token), tokenOffset(token));
            }
            break; }
        case Container:
        case Global:
            if (tokenKind(mCurrentToken - 1) == clang::tok::raw_identifier) {
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
            case clang::tok::equal:
            case clang::tok::r_paren: {
                const int pending = mState.top().pendingIndex;
                assert(pending != -1);
                const clang::Token &token = mTokens.at(pending);
                int offset = tokenOffset(token);
                ByteArray name = tokenSpelling(token);
                if (tokenKind(pending - 1) == clang::tok::tilde) {
                    --offset;
                    name.prepend('~');
                }
                addEntry(name, mContainerScope, offset);
                break; }
            default:
                break;
            }
            mState.pop();
            break;
        default:
            break;
        }
    }
    inline void addContext(int idx, ByteArray &ctx) const
    {
        const int old = idx;
        // printf("Calling addContext cur %s -1 %s -2  %s\n",
        //        clang::tok::getTokenName(tokenKind(idx)),
        //        clang::tok::getTokenName(tokenKind(idx - 1)),
        //        clang::tok::getTokenName(tokenKind(idx - 2)));
        while (idx >= 2 && tokenKind(idx - 1) == clang::tok::coloncolon && tokenKind(idx - 2) == clang::tok::raw_identifier) {
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

    inline clang::tok::TokenKind tokenKind(int idx) const // index <= 0 means from end
    {
        if (idx <= 0) {
            return mTokens.at(mTokens.size() - 1 + idx).getKind();
        } else {
            return mTokens.at(idx).getKind();
        }
    }

    inline void tokenSpelling(const clang::Token &token, const char *&string, int &length) const
    {
        string = mBuf + tokenOffset(token);
        length = token.getLength();
    }

    inline ByteArray tokenSpelling(const clang::Token &token) const
    {
        return ByteArray(mBuf + tokenOffset(token), token.getLength());
    }

    static inline int tokenOffset(const clang::Token &token)
    {
        enum { MacroIDBit = 1U << 31 };
        return token.getLocation().getRawEncoding() & ~MacroIDBit;
    }


    Path mFileName;
    clang::Lexer *mLexer;
    int mBraceCount, mSize, mCurrentToken;
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
    List<clang::Token> mTokens;
    ScopedDB mDB;
};

#endif
