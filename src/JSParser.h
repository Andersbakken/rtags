#ifndef JSParser_h
#define JSParser_h

#include <RTags.h>
#include <Location.h>
#include <CursorInfo.h>
#include <rct/Log.h>
#include <rct/Map.h>
#include <rct/Path.h>
#include <rct/String.h>
#include <v8.h>

class JSParser
{
public:
    JSParser();
    ~JSParser();
    bool init();

    bool parse(const Path &path, const String &contents,
               SymbolMap *cursors,
               SymbolNameMap *symbolNames,
               String *errors,
               String *json = 0);
private:
    enum Flag {
        None = 0x0,
        Scope = 0x1
    };
    enum HandlerType {
        NoHandlerType,
        ArrayExpression,
        AssignmentExpression,
        BinaryExpression,
        BlockStatement,
        BreakStatement,
        CallExpression,
        CatchClause,
        ConditionalExpression,
        ContinueStatement,
        DoWhileStatement,
        EmptyStatement,
        ExpressionStatement,
        ForInStatement,
        ForStatement,
        FunctionDeclaration,
        FunctionExpression,
        Identifier,
        IfStatement,
        Literal,
        LogicalExpression,
        MemberExpression,
        NewExpression,
        ObjectExpression,
        Program,
        Property,
        ReturnStatement,
        ThisExpression,
        ThrowStatement,
        TryStatement,
        UnaryExpression,
        UpdateExpression,
        VariableDeclaration,
        VariableDeclarator,
        WhileStatement
    };

    bool handleObject(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleIdentifier(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleArrayExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleAssignmentExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleBinaryExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleBlockStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleBreakStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleCallExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleCatchClause(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleConditionalExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleContinueStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleDoWhileStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleEmptyStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleExpressionStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleForInStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleForStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleFunctionDeclaration(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleFunctionExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleIfStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleLiteral(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleLogicalExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleMemberExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleNewExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleObjectExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleProgram(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleProperty(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleReturnStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleThisExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleThrowStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleTryStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleUnaryExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleUpdateExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleVariableDeclaration(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleVariableDeclarator(v8::Handle<v8::Object> object, const String &name, unsigned flags);
    void handleWhileStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags);

    enum CreateSymbolFlag {
        NoCreateSymbolFlag = 0x0,
        AddToParents = 0x1,
        IgnoreLastParent = 0x2,
        OnlyLastParent = 0x4
    };
    void createSymbol(v8::Handle<v8::Object> object, CursorInfo::JSCursorKind kind, unsigned flags = NoCreateSymbolFlag);
    void handleProperties(v8::Handle<v8::Object> object, const String &name, unsigned flags);

    typedef void (JSParser::*Handler)(v8::Handle<v8::Object>, const String &name, unsigned);

    struct HandlerNode
    {
        const char *name;
        HandlerType type;
        Handler handler;
    };

    static int compareHandler(const void *l, const void *r);

    struct State {
        HandlerType type;
        String name;
    };
    List<State> mState;

    v8::Persistent<v8::Context> mContext;
    v8::Persistent<v8::Object> mEsprima;
    v8::Persistent<v8::Function> mParse;
    v8::Isolate *mIsolate;
    List<Map<String, uint32_t> > mScope;
    List<String> mParents;
    uint32_t mFileId;
    SymbolMap *mSymbols;
    SymbolNameMap *mSymbolNames;
    String *mErrors;
};


#endif
