#include "JSParser.h"
#include <v8.h>
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>

#define toCString(str) *v8::String::Utf8Value(str)

static v8::Handle<v8::String> toJSON(v8::Handle<v8::Value> obj, bool pretty)
{
    v8::HandleScope scope;

    v8::Handle<v8::Context> context = v8::Context::GetCurrent();
    v8::Handle<v8::Object> global = context->Global();

    v8::Handle<v8::Object> JSON = global->Get(v8::String::New("JSON"))->ToObject();
    v8::Handle<v8::Function> JSON_stringify = v8::Handle<v8::Function>::Cast(JSON->Get(v8::String::New("stringify")));

    v8::Handle<v8::Value> args[3];
    args[0] = obj;
    args[1] = v8::Null();
    args[2] = v8::String::New("    ");

    return scope.Close(JSON_stringify->Call(JSON, pretty ? 3 : 2, args)->ToString());
}

Log operator<<(Log log, v8::Handle<v8::String> string)
{
    log << toCString(string);
    return log;
}

Log operator<<(Log log, v8::Handle<v8::Value> value)
{
    log << toCString(toJSON(value, true));
    return log;
}

template <typename T>
static v8::Handle<T> get(v8::Handle<v8::Object> object, v8::Handle<v8::String> property)
{
    if (object.IsEmpty() || !object->IsObject())
        return v8::Handle<T>();
    v8::HandleScope scope;
    v8::Handle<v8::Value> prop(object->Get(property));
    if (prop.IsEmpty() || prop->IsNull() || prop->IsUndefined()) {
        return scope.Close(v8::Handle<T>());
    } else {
        return scope.Close(v8::Handle<T>::Cast(prop));
    }
}

template <typename T>
static v8::Handle<T> get(v8::Handle<v8::Object> object, const char *property)
{
    return get<T>(object, v8::String::New(property));
}

template <typename T>
static v8::Persistent<T> getPersistent(v8::Handle<v8::Object> object, v8::Handle<v8::String> property)
{
    v8::Persistent<T> prop(get<T>(object, property));
    return prop;
}

template <typename T>
static v8::Persistent<T> getPersistent(v8::Handle<v8::Object> object, const char *property)
{
    return getPersistent<T>(object, v8::String::New(property));
}

template <typename T>
static v8::Handle<T> get(v8::Handle<v8::Array> object, int index)
{
    if (object.IsEmpty() || !object->IsArray())
        return v8::Handle<T>();
    v8::HandleScope scope;
    v8::Handle<v8::Value> prop = object->Get(index);
    if (prop.IsEmpty() || prop->IsNull() || prop->IsUndefined()) {
        return scope.Close(v8::Handle<T>());
    } else {
        return scope.Close(v8::Handle<T>::Cast(prop));
    }
}

static String listProperties(v8::Handle<v8::Object> obj)
{
    List<String> ret;
    if (!obj.IsEmpty() && obj->IsObject()) {
        v8::HandleScope scope;
        v8::Handle<v8::Array> props = obj->GetOwnPropertyNames();
        for (unsigned i=0; i<props->Length(); ++i) {
            ret += toCString(get<v8::String>(props, i));
        }
    }
    return String::join(ret, ", ");
}

static inline bool operator==(v8::Handle<v8::String> l, const char *r)
{
    // error() << "comparing" << (l.IsEmpty() ? "empty" : toCString(l)) << r;
    return l.IsEmpty() ? (!r || !strlen(r)) : !strcmp(toCString(l), r);
}

static inline bool operator==(const char *l, v8::Handle<v8::String> r)
{
    return operator==(r, l);
}

static inline bool operator!=(const char *l, v8::Handle<v8::String> r)
{
    return !operator==(r, l);
}

static inline bool operator!=(v8::Handle<v8::String> l, const char *r)
{
    return !operator==(l, r);
}

int indent = 0;

static String indentString()
{
    return String(indent * 2, ' ');
}

JSParser::JSParser()
    : mIsolate(0), mFileId(0), mSymbols(0), mSymbolNames(0), mErrors(0)
{}

JSParser::~JSParser()
{
    {
        const v8::Isolate::Scope isolateScope(mIsolate);
#ifdef V8_DISPOSE_REQUIRES_ARG
        if (!mParse.IsEmpty())
            mParse.Dispose(mIsolate);
        if (!mEsprima.IsEmpty())
            mEsprima.Dispose(mIsolate);
        if (!mContext.IsEmpty())
            mContext.Dispose(mIsolate);
#else
        if (!mParse.IsEmpty())
            mParse.Dispose();
        if (!mEsprima.IsEmpty())
            mEsprima.Dispose();
        if (!mContext.IsEmpty())
            mContext.Dispose();
#endif
    }
    mIsolate->Dispose();
}

bool JSParser::init()
{
    mIsolate = v8::Isolate::New();
    const v8::Isolate::Scope isolateScope(mIsolate);
    v8::HandleScope handleScope;
    mContext = v8::Context::New();
    v8::Context::Scope scope(mContext);
    assert(!mContext.IsEmpty());

    const String esprimaSrcString = Path(ESPRIMA_JS).readAll();
    v8::Handle<v8::String> esprimaSrc = v8::String::New(esprimaSrcString.constData(), esprimaSrcString.size());

    v8::TryCatch tryCatch;
    v8::Handle<v8::Script> script = v8::Script::Compile(esprimaSrc);
    if (tryCatch.HasCaught() || script.IsEmpty() || !tryCatch.Message().IsEmpty()) {
        v8::Handle<v8::Message> message = tryCatch.Message();
        v8::String::Utf8Value msg(message->Get());
        printf("%s:%d:%d: esprima error: %s {%d-%d}\n", ESPRIMA_JS, message->GetLineNumber(),
               message->GetStartColumn(), *msg, message->GetStartPosition(), message->GetEndPosition());
        return false;
    }
    script->Run();

    v8::Handle<v8::Object> global = mContext->Global();
    assert(!global.IsEmpty());
    mEsprima = getPersistent<v8::Object>(global, "esprima");
    if (mEsprima.IsEmpty() || !mEsprima->IsObject()) {
        return false;
    }
    mParse = getPersistent<v8::Function>(mEsprima, "parse");

    return !mParse.IsEmpty() && mParse->IsFunction();
}

bool JSParser::parse(const Path &path, const String &contents, SymbolMap *symbols, SymbolNameMap *symbolNames,
                     String *errors, String *json)
{
    const v8::Isolate::Scope isolateScope(mIsolate);
    mFileId = Location::insertFile(path);
    v8::HandleScope handleScope;
    v8::Context::Scope scope(mContext);
    String tmp;
    if (contents.isEmpty())
        tmp = path.readAll();
    const String &c = contents.isEmpty() ? tmp : contents;
    if (c.isEmpty()) {
        printf("[%s] %s:%d: if (c.isEmpty()) { [after]\n", __func__, __FILE__, __LINE__);
        return false;
    }
    v8::Handle<v8::Value> args[2];
    // args[0] = v8::String::New(file.constData(), file.size());
    args[0] = v8::String::New(c.constData(), c.size());
    v8::Handle<v8::Object> options = v8::Object::New();
    options->Set(v8::String::New("range"), v8::Boolean::New(true));
    options->Set(v8::String::New("tolerant"), v8::Boolean::New(true));
    args[1] = options;
    assert(!mEsprima.IsEmpty() && mEsprima->IsObject());
    assert(!mParse.IsEmpty() && mParse->IsFunction());
    assert(!args[0].IsEmpty() && args[0]->IsString());
    assert(!args[1].IsEmpty() && args[1]->IsObject());
    v8::Handle<v8::Value> result = mParse->Call(mEsprima, 2, args);

    mSymbols = symbols;
    mSymbolNames = symbolNames;
    mErrors = errors;
    if (!result.IsEmpty() && result->IsObject()) {
        if (json)
            *json = toCString(toJSON(result, true));
        mScope.append(Map<String, uint32_t>());
        handleObject(get<v8::Object>(result->ToObject(), "body"), "body", Scope);
        mScope.removeLast();
    } else if (errors) {
        *errors = "Failed to parse";
    }
    // for (Map<int, CursorInfo>::const_iterator it = symbols.begin(); it != symbols.end(); ++it) {
    //     error() << String::format<64>("%s,%d", path.constData(), it->first) << it->second;
    // }
    mSymbols = 0;
    mSymbolNames = 0;
    mErrors = 0;
    assert(mScope.isEmpty());
    if (!mParents.isEmpty()) {
        error() << "Got leftover parents" << mParents;
    }
    assert(mParents.isEmpty());
    return true;
}

void JSParser::createSymbol(v8::Handle<v8::Object> object, CursorInfo::JSCursorKind kind, unsigned flags)
{
    const v8::Isolate::Scope isolateScope(mIsolate);
    v8::HandleScope handleScope;
    v8::Context::Scope scope(mContext);
    v8::Handle<v8::String> name = get<v8::String>(object, "name");
    v8::Handle<v8::Array> range = get<v8::Array>(object, "range");
    assert(!range.IsEmpty() && range->Length() == 2);
    const uint32_t offset = get<v8::Integer>(range, 0)->Value();
    const uint32_t length = get<v8::Integer>(range, 1)->Value() - offset;
    CursorInfo c;
    const Location loc(mFileId, offset);
    const String symbolName(toCString(name), name->Length());
    int count = mParents.size();
    if (flags & IgnoreLastParent && count)
        --count;
    int i = 0;
    if (flags & OnlyLastParent && count)
        i = count - 1;
    while (i < count) {
        c.symbolName += mParents.at(i);
        c.symbolName += ".";
        ++i;
    }

    c.symbolName += symbolName;
    c.symbolLength = length;
    c.kind = kind;
    if (c.kind == CursorInfo::JSReference || c.kind == CursorInfo::JSWeakVariable) {
        for (int i=mScope.size() - 1; i>=0; --i) {
            uint32_t targetOffset = mScope.at(i).value(c.symbolName, UINT_MAX);
            // error() << "looking for" << c.symbolName << "in" << i << mScope.at(i).keys() << (targetOffset != UINT_MAX);
            if (targetOffset != UINT_MAX) {
                const Location target(mFileId, targetOffset);
                c.targets.insert(target);
                if (mSymbols) {
                    assert(mSymbols->contains(target));
                    (*mSymbols)[target].references.insert(loc);
                }
                c.kind = CursorInfo::JSReference;
                break;
            }
        }
    }
    if (mSymbols)
        (*mSymbols)[loc] = c;
    if (c.kind != CursorInfo::JSReference) {
        if (mSymbolNames) {
            (*mSymbolNames)[symbolName].insert(loc);
            (*mSymbolNames)[c.symbolName].insert(loc);
        }

        mScope.last()[c.symbolName] = offset;
    }
    if (flags & AddToParents)
        mParents.append(symbolName);
    error() << indentString() << "+++ adding" << c.kindSpelling() << c.symbolName << "at" << loc << "scope";
}

int JSParser::compareHandler(const void *l, const void *r)
{
    const HandlerNode *left = reinterpret_cast<const HandlerNode*>(l);
    const HandlerNode *right = reinterpret_cast<const HandlerNode*>(r);
    assert(left->name);
    assert(right->name);
    return strcmp(left->name, right->name);
}

bool JSParser::handleObject(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    if (object.IsEmpty())
        return false;

    ++indent;
    const bool scope = (flags & Scope);
    if (scope) {
        flags &= ~Scope;
        error() << indentString() << "adding a scope";
        mScope.append(Map<String, uint32_t>());
    }
    if (object->IsArray()) {
        v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(object);
        for (unsigned i=0; i<array->Length(); ++i) {
            handleObject(get<v8::Object>(array, i), "", flags);
        }
    } else if (object->IsObject()) {
        v8::Handle<v8::String> objectType = get<v8::String>(object, "type");
        if (!objectType.IsEmpty()) {
            assert(objectType->IsString());
            static const HandlerNode handlers[] = {
                { "ArrayExpression", ArrayExpression, &JSParser::handleArrayExpression },
                { "AssignmentExpression", AssignmentExpression, &JSParser::handleAssignmentExpression },
                { "BinaryExpression", BinaryExpression, &JSParser::handleBinaryExpression },
                { "BlockStatement", BlockStatement, &JSParser::handleBlockStatement },
                { "BreakStatement", BreakStatement, &JSParser::handleBreakStatement },
                { "CallExpression", CallExpression, &JSParser::handleCallExpression },
                { "CatchClause", CatchClause, &JSParser::handleCatchClause },
                { "ConditionalExpression", ConditionalExpression, &JSParser::handleConditionalExpression },
                { "ContinueStatement", ContinueStatement, &JSParser::handleContinueStatement },
                { "DoWhileStatement", DoWhileStatement, &JSParser::handleDoWhileStatement },
                { "EmptyStatement", EmptyStatement, &JSParser::handleEmptyStatement },
                { "ExpressionStatement", ExpressionStatement, &JSParser::handleExpressionStatement },
                { "ForInStatement", ForInStatement, &JSParser::handleForInStatement },
                { "ForStatement", ForStatement, &JSParser::handleForStatement },
                { "FunctionDeclaration", FunctionDeclaration, &JSParser::handleFunctionDeclaration },
                { "FunctionExpression", FunctionExpression, &JSParser::handleFunctionExpression },
                { "Identifier", Identifier, &JSParser::handleIdentifier },
                { "IfStatement", IfStatement, &JSParser::handleIfStatement },
                { "Literal", Literal, &JSParser::handleLiteral },
                { "LogicalExpression", LogicalExpression, &JSParser::handleLogicalExpression },
                { "MemberExpression", MemberExpression, &JSParser::handleMemberExpression },
                { "NewExpression", NewExpression, &JSParser::handleNewExpression },
                { "ObjectExpression", ObjectExpression, &JSParser::handleObjectExpression },
                { "Program", Program, &JSParser::handleProgram },
                { "Property", Property, &JSParser::handleProperty },
                { "ReturnStatement", ReturnStatement, &JSParser::handleReturnStatement },
                { "ThisExpression", ThisExpression, &JSParser::handleThisExpression },
                { "ThrowStatement", ThrowStatement, &JSParser::handleThrowStatement },
                { "TryStatement", TryStatement, &JSParser::handleTryStatement },
                { "UnaryExpression", UnaryExpression, &JSParser::handleUnaryExpression },
                { "UpdateExpression", UpdateExpression, &JSParser::handleUpdateExpression },
                { "VariableDeclaration", VariableDeclaration, &JSParser::handleVariableDeclaration },
                { "VariableDeclarator", VariableDeclarator, &JSParser::handleVariableDeclarator },
                { "WhileStatement", WhileStatement, &JSParser::handleWhileStatement },
                { 0, NoHandlerType, 0 }
            };
            const v8::String::Utf8Value str(objectType);
            const HandlerNode node = { *str, NoHandlerType, 0 };
            enum { NodeSize = sizeof(HandlerNode) };
            const HandlerNode *res = reinterpret_cast<const HandlerNode*>(bsearch(&node, handlers,
                                                                                  (sizeof(handlers) - NodeSize) / NodeSize,
                                                                                  NodeSize, &JSParser::compareHandler));
            assert(res);
            assert(res->name);
            const State state = { res->type, name };
            mState.append(state);
            (this->*res->handler)(object, name, flags);
            mState.removeLast();
        }
    }
    if (scope) {
        error() << indentString() << "popping a scope";
        mScope.removeLast();
    }

    --indent;

    return true;
}

void JSParser::handleProperties(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    v8::Handle<v8::Array> properties = object->GetOwnPropertyNames();

    for (unsigned i=0; i<properties->Length(); ++i) {
        v8::Handle<v8::String> key = get<v8::String>(properties, i);
        v8::Handle<v8::Object> value = get<v8::Object>(object, key);
        if (!value.IsEmpty()) {
            const v8::String::Utf8Value utf8(key);
            unsigned f = flags;
            if (!strcmp(*utf8, "body"))
                f |= Scope;
            handleObject(value, *utf8, f);
        }
    }
}


void JSParser::handleIdentifier(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "Identifier" << name << toJSON(object, false);
    assert(mState.size() > 1);
    switch (mState.at(mState.size() - 2).type) {
    case VariableDeclarator:
        if (name == "id") {
            createSymbol(object, CursorInfo::JSVariable, AddToParents);
        } else if (name == "init") {
            createSymbol(object, CursorInfo::JSWeakVariable, IgnoreLastParent);
        } else {
            error() << "______________ WHAT TO DO WITH THIS? DOES IT HAPPEN?";
        }
        break;
    case FunctionDeclaration:
        createSymbol(object, CursorInfo::JSFunction);
        break;
    case UpdateExpression:
        createSymbol(object, CursorInfo::JSWeakVariable);
        break;
    case AssignmentExpression:
        // assert(name == "left");
        if (name == "left") {
            createSymbol(object, CursorInfo::JSWeakVariable, AddToParents);
        } else {
            createSymbol(object, CursorInfo::JSWeakVariable, AddToParents);
        }
        break;
    case Property:
        if (mState.size() > 2 && mState.at(mState.size() - 3).type == ObjectExpression) {
            createSymbol(object, CursorInfo::JSVariable, name == "key" ? AddToParents : NoCreateSymbolFlag);
        } else {
            createSymbol(object, CursorInfo::JSWeakVariable, name == "key" ? AddToParents : NoCreateSymbolFlag);
        }
        break;
    case MemberExpression:
        if (name == "object") {
            unsigned f = AddToParents;
            if (mState.size() > 2 && mState.at(mState.size() - 3).type == VariableDeclarator && mState.at(mState.size() - 2).name == "init")
                f |= IgnoreLastParent;

            createSymbol(object, CursorInfo::JSWeakVariable, f);
        } else {
            assert(name == "property");
            createSymbol(object, CursorInfo::JSWeakVariable, OnlyLastParent);
        }
        break;
    default:
        break;
    }
}

class RestoreParents
{
public:
    RestoreParents(List<String> &parents)
        : mParents(parents), mSize(parents.size())
    {
    }
    ~RestoreParents()
    {
        mParents.truncate(mSize);
    }
private:
    List<String> &mParents;
    const int mSize;
};

void JSParser::handleArrayExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ArrayExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleAssignmentExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    RestoreParents scope(mParents);
    error() << indentString() << "AssignmentExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleBinaryExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "BinaryExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleBlockStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "BlockStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleBreakStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "BreakStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleCallExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "CallExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleCatchClause(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "CatchClause" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleConditionalExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ConditionalExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleContinueStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ContinueStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleDoWhileStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "DoWhileStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleEmptyStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "EmptyStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleExpressionStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ExpressionStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleForInStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ForInStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleForStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ForStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleFunctionDeclaration(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "FunctionDeclaration" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleFunctionExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "FunctionExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleIfStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "IfStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleLiteral(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "Literal" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleLogicalExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "LogicalExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleMemberExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    RestoreParents scope(mParents);
    error() << indentString() << "MemberExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleNewExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "NewExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleObjectExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ObjectExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleProgram(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "Program" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleProperty(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    RestoreParents scope(mParents);
    error() << indentString() << "Property" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleReturnStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ReturnStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleThisExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ThisExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleThrowStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "ThrowStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleTryStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "TryStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleUnaryExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "UnaryExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleUpdateExpression(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "UpdateExpression" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleVariableDeclaration(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "VariableDeclaration" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleVariableDeclarator(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    RestoreParents scope(mParents);
    error() << indentString() << "VariableDeclarator" << name << listProperties(object);
    handleProperties(object, name, flags);
}

void JSParser::handleWhileStatement(v8::Handle<v8::Object> object, const String &name, unsigned flags)
{
    error() << indentString() << "WhileStatement" << name << listProperties(object);
    handleProperties(object, name, flags);
}
