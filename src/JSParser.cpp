#include "JSParser.h"
#include <v8.h>
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>

v8::Handle<v8::String> toJson(v8::Handle<v8::Value> obj)
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

    return scope.Close(JSON_stringify->Call(JSON, 3, args)->ToString());
}

template <typename T>
static v8::Handle<T> get(v8::Handle<v8::Object> object, v8::Handle<v8::String> property)
{
    v8::HandleScope scope;
    v8::Handle<v8::Value> prop(object->Get(property));
    return scope.Close(v8::Handle<T>::Cast(prop));
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
    v8::HandleScope scope;
    v8::Handle<v8::Value> prop = object->Get(index);
    return scope.Close(v8::Handle<T>::Cast(prop));
}

static inline bool operator==(v8::Handle<v8::String> l, const char *r)
{
    return l.IsEmpty() ? (!r || !strlen(r)) : !strcmp(*v8::String::Utf8Value(l), r);
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

#define toCString(str) *v8::String::Utf8Value(str)

JSParser::~JSParser()
{
    // mParse.Dispose(v8::Isolate::GetCurrent()); // the one underneath got deprecated at some point but I don't know when
    // mEsprima.Dispose(v8::Isolate::GetCurrent());
    // mContext.Dispose(v8::Isolate::GetCurrent());
    {
        const v8::Isolate::Scope isolateScope(mIsolate);
        mParse.Dispose();
        mEsprima.Dispose();
        mContext.Dispose();
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
    v8::Handle<v8::Value> args[2];
    // args[0] = v8::String::New(file.constData(), file.size());
    args[0] = v8::String::New(c.constData(), c.size());
    v8::Handle<v8::Object> options = v8::Object::New();
    options->Set(v8::String::New("range"), v8::Boolean::New(true));
    options->Set(v8::String::New("tolerant"), v8::Boolean::New(true));
    args[1] = options;
    v8::Handle<v8::Value> result = mParse->Call(mEsprima, 2, args);

    mSymbols = symbols;
    mSymbolNames = symbolNames;
    mErrors = errors;
    if (!result.IsEmpty()) {
        if (json)
            *json = toCString(toJson(result));

        visit(result->ToObject());
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
    assert(mParents.isEmpty());
    return true;
}

bool JSParser::visit(v8::Handle<v8::Object> object)
{
    assert(!object.IsEmpty());
    // v8::Handle<v8::Value> b = parse->Get(v8::String::New("body"));
    // printf("isnull %d\n", b.IsEmpty());
    // printf("foo[%s]\n", toCString(toJson(b)));

    v8::Handle<v8::Array> body = get<v8::Array>(object, "body");
    if (body.IsEmpty()) {
        return false;
    }

    mScope.append(Map<String, uint32_t>());
    for (unsigned i=0; i<body->Length(); ++i) {
        if (!visitBlock(get<v8::Object>(body, i), NoFlag)) {
            error("Invalid body element at index %d", i);
        }
    }
    mScope.removeLast();
    return true;
}

bool JSParser::visitIdentifier(v8::Handle<v8::Object> identifier, CursorInfo::JSCursorKind kind)
{
    if (identifier.IsEmpty() || !identifier->IsObject()) {
        return false;
    }
    // v8::Handle<v8::Array> props = identifier->GetOwnPropertyNames();
    // for (unsigned i=0; i<props->Length(); ++i) {
    //     v8::Handle<v8::String> str = get<v8::String>(props, i);
    //     error() << "prop" << i << *v8::String::Utf8Value(str);
    // }

    v8::Handle<v8::String> name = get<v8::String>(identifier, "name");
    v8::Handle<v8::Array> range = get<v8::Array>(identifier, "range");
    assert(!range.IsEmpty() && range->Length() == 2);
    const uint32_t offset = get<v8::Integer>(range, 0)->Value();
    const uint32_t length = get<v8::Integer>(range, 1)->Value() - offset;
    CursorInfo c;
    c.kind = kind;
    for (int i=0; i<mParents.size(); ++i) {
        c.symbolName += mParents.at(i);
        c.symbolName += '.';
    }

    const Location loc(mFileId, offset);
    c.symbolName += String(toCString(name), name->Length());
    if (c.kind == CursorInfo::JSReference || c.kind == CursorInfo::JSWeakVariable) {
        for (int i=mScope.size() - 1; i>=0; --i) {
            uint32_t targetOffset = mScope.at(i).value(c.symbolName, UINT_MAX);
            if (targetOffset != UINT_MAX) {
                const Location target(mFileId, targetOffset);
                c.targets.insert(target);
                if (mSymbols) {
                    assert(mSymbols->contains(target));
                    (*mSymbols)[target].references.insert(loc);
                }
                if (c.kind == CursorInfo::JSWeakVariable) {
                    c.kind = CursorInfo::JSReference;
                }
                break;
            }
        }
    }
    c.symbolLength = length;
    if (mSymbols) {
        (*mSymbols)[loc] = c;
    }
    // error() << "adding" << c << "at" << offset << "scope" << mScope.last().keys();
    if (c.kind != CursorInfo::JSReference) {
        mScope.last()[c.symbolName] = offset;
        if (mSymbolNames)
            (*mSymbolNames)[c.symbolName].insert(loc);
    }
    return true;
}

bool JSParser::visitBlock(v8::Handle<v8::Object> object, unsigned flags)
{
    v8::HandleScope handleScope;
    if (object.IsEmpty() || !object->IsObject())
        return false;
    assert(object->IsObject());

    v8::Handle<v8::String> type = get<v8::String>(object, "type");
    // error() << "visitBlock" << toCString(type);
    assert(!type.IsEmpty());
    if (type == "FunctionDeclaration") {
        visitIdentifier(get<v8::Object>(object, "id"), CursorInfo::JSFunction);
    } else if (type == "VariableDeclaration") {
        v8::Handle<v8::Array> declarations = get<v8::Array>(object, "declarations");
        if (!declarations.IsEmpty() && declarations->IsArray()) {
            for (unsigned i=0; i<declarations->Length(); ++i) {
                v8::Handle<v8::Object> declarator = get<v8::Object>(declarations, i);
                if (get<v8::String>(declarator, "type") == "VariableDeclarator") {
                    visitIdentifier(get<v8::Object>(declarator, "id"), CursorInfo::JSVariable);
                }
            }
        }
    } else if (type == "Identifier") {
        visitIdentifier(object, flags & TreatRefsAsWeakVariables ? CursorInfo::JSWeakVariable : CursorInfo::JSReference);
    } else {
        bool popObjectScope = false;
        unsigned f = flags;
        if (type == "MemberExpression") {
            v8::Handle<v8::Object> obj = get<v8::Object>(object, "object");
            if (!obj.IsEmpty() && obj->IsObject()) {
                v8::Handle<v8::String> objName = get<v8::String>(obj, "name");
                if (!objName.IsEmpty() && objName->IsString()) {
                    visitIdentifier(obj, flags & TreatRefsAsWeakVariables ? CursorInfo::JSWeakVariable : CursorInfo::JSReference);
                    mParents.append(String(toCString(objName), objName->Length()));
                    popObjectScope = true;
                    f |= TreatRefsAsWeakVariables;
                }
            }
        } else if (type == "AssignmentExpression") {
            f |= TreatRefsAsWeakVariables;
        }

        v8::Handle<v8::Array> properties = object->GetOwnPropertyNames();
        if (!properties.IsEmpty() && properties->IsArray()) {
            for (unsigned i=0; i<properties->Length(); ++i) {
                v8::Handle<v8::String> property = get<v8::String>(properties, i);
                // error() << "visiting" << toCString(property);
                if (property != "type"
                    && property != "body"
                    && property != "Identifier"
                    && (!popObjectScope || property != "object")) {
                    visitBlock(get<v8::Object>(object, toCString(property)), f);
                }
            }
        }
        if (popObjectScope)
            mParents.removeLast();
    }
    v8::Handle<v8::Value> bodyValue = object->Get(v8::String::New("body"));
    if (!bodyValue.IsEmpty()) {
        mScope.append(Map<String, uint32_t>());
        if (bodyValue->IsArray()) {
            v8::Handle<v8::Array> body = v8::Handle<v8::Array>::Cast(bodyValue);
            for (unsigned i=0; i<body->Length(); ++i) {
                visitBlock(get<v8::Object>(body, i), flags);
            }
        } else if (bodyValue->IsObject()) {
            v8::Handle<v8::Object> body = v8::Handle<v8::Object>::Cast(bodyValue);
            v8::Handle<v8::Array> properties = body->GetOwnPropertyNames();
            if (!properties.IsEmpty() && properties->IsArray()) {
                for (unsigned i=0; i<properties->Length(); ++i) {
                    v8::Handle<v8::String> property = get<v8::String>(properties, i);
                    visitBlock(get<v8::Object>(body, property), flags);
                }
            }
        }
        mScope.removeLast();
    }
    return true;
}
