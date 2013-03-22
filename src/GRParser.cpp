#include "GRParser.h"

static bool isKeyWord(const char *tokenSpl, int tokenLength)
{
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
    case 10:
        keyWord = !strncmp(tokenSpl, "const_cast", 10);
        break;
    case 11:
        keyWord = !strncmp(tokenSpl, "static_cast", 11);
        break;
    case 12:
        keyWord = !strncmp(tokenSpl, "dynamic_cast", 12);
        break;
    case 16:
        keyWord = !strncmp(tokenSpl, "reinterpret_cast", 16);
        break;
        // we could add sizeof and typeid here but it's kinda neat to find references to them
    }
    return keyWord;
}

GRParser::GRParser()
    : mLexer(0), mBraceCount(0), mSize(0), mCount(0), mCurrentToken(0), mBuf(0), mFileId(0), mEntries(0)
{}

GRParser::~GRParser()
{
    delete mLexer;
    delete[] mBuf;
}

int GRParser::parse(const Path &file, unsigned opts, Map<String, Map<Location, bool> > &entries)
{
    mFileName = file;
    mSize = mFileName.readAll(mBuf);
    switch (mSize) {
    case 0:
        return 0;
    case -1:
        error("Read error %s", mFileName.constData());
        return 0;
    default:
        break;
    }
    mEntries = &entries;
    mFileId = Location::insertFile(file);
    clang::SourceLocation loc;
    clang::LangOptions options;
    if (opts & CPlusPlus) {
        options.CPlusPlus = true;
        // options.CPlusPlus11 = true; // not sure when this was added to clang so I don't know what to do with the ifdef
    }
    mLexer = new clang::Lexer(loc, options, mBuf, mBuf, mBuf + mSize);
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
    mEntries = 0;
    return mCount;
}

void GRParser::addEntry(const String &name, const List<String> &containerScope, int offset)
{
    ++mCount;
    const Location loc(mFileId, offset);
    (*mEntries)[name][loc] = false;
    if (!containerScope.isEmpty()) {
        String entry = name;
        entry.reserve(256); // ### does this help for prepend?
        for (int i=containerScope.size() - 1; i>=0; --i) {
            entry.prepend("::");
            entry.prepend(containerScope.at(i));
            (*mEntries)[entry][loc] = false;
        }
    }
}

void GRParser::addReference(const String &name, int offset)
{
    ++mCount;
    const Location loc(mFileId, offset);
    (*mEntries)[name][loc] = true;
}

int GRParser::findMatching(int idx) const
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

void GRParser::handleRawIdentifier()
{
    switch (mState.top().type) {
    case Global:
    case Container:
    case FunctionBody:
        switch (kind(mCurrentToken - 1)) {
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

void GRParser::handleLeftBrace()
{
    switch (mState.top().type) {
    case ContainerPending: {
        assert(!mState.empty());
        int containerIndex = mState.top().pendingIndex;
        if (containerIndex == -1 && kind(mCurrentToken - 1) == clang::tok::raw_identifier)
            containerIndex = mCurrentToken - 1;
        mState.pop();
        if (containerIndex != -1) {
            const clang::Token &token = mTokens[containerIndex];
            const String name = tokenSpelling(token);
            if (!isKeyWord(name.constData(), name.size())) {
                const int added = addContext(containerIndex);
                addEntry(tokenSpelling(token), mContainerScope, tokenOffset(token));
                mContainerScope.chop(added);
                mState.push(State(Container, mBraceCount, name));
                mContainerScope.append(name);
            }
        }
        break; }
    case FunctionPending: {
        int function = mState.top().pendingIndex;
        assert(function != -1);
        const clang::Token &token = mTokens.at(function);
        int offset = tokenOffset(token);
        String name = tokenSpelling(token);
        if (!isKeyWord(name.constData(), name.size())) {

            if (kind(function - 1) == clang::tok::tilde) {
                name.prepend('~');
                --offset;
                --function;
            }
            const int added = addContext(function);
            addEntry(name, mContainerScope, offset);
            mContainerScope.chop(added);
        }
        mState.pop();
        mState.push(State(FunctionBody, mBraceCount));
        break; }
    default:
        break;
    }
    ++mBraceCount;
}

void GRParser::handleColon()
{
    if (mState.top().type == ContainerPending
        && mState.top().pendingIndex == -1
        && kind(mCurrentToken - 1) == clang::tok::raw_identifier) {
        mState.top().pendingIndex = mCurrentToken - 1;
    }
}

void GRParser::handleLeftParen()
{
    switch (mState.top().type) {
    case FunctionBody: {
        int idx = -1;
        switch (kind(mCurrentToken - 1)) {
        case clang::tok::raw_identifier:
            idx = mCurrentToken - 1;
            break;
        case clang::tok::greater:
            idx = findMatching(mCurrentToken - 1);
            if (kind(idx - 1) == clang::tok::raw_identifier) {
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

            if (!isKeyWord(tokenSpl, tokenLength))
                addReference(tokenSpelling(token), tokenOffset(token));
        }
        break; }
    case Container:
    case Global:
        if (kind(mCurrentToken - 1) == clang::tok::raw_identifier) {
            State state(FunctionPending);
            state.pendingIndex = mCurrentToken - 1;
            mState.push(state);
        }
        break;
    default:
        break;
    }
}
void GRParser::handleRightBrace()
{
    if (mBraceCount > 0)
        --mBraceCount;
    if (mState.top().braceIndex == mBraceCount) {
        const State &top = mState.top();
        if (top.type == Container) {
            assert(!top.name.isEmpty());
            mContainerScope.resize(mContainerScope.size() - 1);
        }
        mState.pop();
        assert(!mState.empty());
    }
}
void GRParser::handleSemi()
{
    switch (mState.top().type) {
    case ContainerPending: // forward declaration
        mState.pop();
        assert(!mState.empty());
        break;
    case FunctionPending:
        switch (kind(mCurrentToken - 1)) {
        case clang::tok::equal:
        case clang::tok::r_paren: {
            const int pending = mState.top().pendingIndex;
            assert(pending != -1);
            const clang::Token &token = mTokens.at(pending);
            int offset = tokenOffset(token);
            String name = tokenSpelling(token);
            if (kind(pending - 1) == clang::tok::tilde) {
                --offset;
                name.prepend('~');
            }
            if (!isKeyWord(name.constData(), name.size()))
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
int GRParser::addContext(int idx)
{
    const int old = idx;
    // printf("Calling addContext cur %s -1 %s -2  %s\n",
    //        clang::tok::getTokenName(kind(idx)),
    //        clang::tok::getTokenName(kind(idx - 1)),
    //        clang::tok::getTokenName(kind(idx - 2)));
    while (idx >= 2 && kind(idx - 1) == clang::tok::coloncolon && kind(idx - 2) == clang::tok::raw_identifier) {
        idx -= 2;
    }
    for (int i=idx; i<old; i+=2) {
        const clang::Token &token = mTokens.at(i);
        mContainerScope.append(tokenSpelling(token));
    }
    return (old - idx) / 2;
}

