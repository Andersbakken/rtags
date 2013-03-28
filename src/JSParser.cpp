#include "JSParser.h"
#include <v8.h>
#include <rct/Mutex.h>
#include <rct/MutexLocker.h>

#define toCString(str) *v8::String::Utf8Value(str)

v8::Handle<v8::String> toJSON(v8::Handle<v8::Value> obj)
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
    v8::HandleScope scope;
    v8::Handle<v8::Value> prop = object->Get(index);
    if (prop.IsEmpty() || prop->IsNull() || prop->IsUndefined()) {
        return scope.Close(v8::Handle<T>());
    } else {
        return scope.Close(v8::Handle<T>::Cast(prop));
    }
}

static inline bool operator==(v8::Handle<v8::String> l, const char *r)
{
    error() << "comparing" << (l.IsEmpty() ? "empty" : toCString(l)) << r;
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

JSParser::~JSParser()
{
    // mParse.Dispose(v8::Isolate::GetCurrent()); // the one underneath got deprecated at some point but I don't know when
    // mEsprima.Dispose(v8::Isolate::GetCurrent());
    // mContext.Dispose(v8::Isolate::GetCurrent());
    {
        const v8::Isolate::Scope isolateScope(mIsolate);
#ifdef V8_DISPOSE_REQUIRES_ARG
        mParse.Dispose(v8::Isolate::GetCurrent());
        mEsprima.Dispose(v8::Isolate::GetCurrent());
        mContext.Dispose(v8::Isolate::GetCurrent());
#else
        mParse.Dispose();
        mEsprima.Dispose();
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
    if (!result.IsEmpty() && result->IsObject()) {
        if (json)
            *json = toCString(toJSON(result));
        mScope.append(Map<String, uint32_t>());
        recurseObject(get<v8::Object>(result->ToObject(), "body"), "body");
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
    assert(mParents.isEmpty());
    return true;
}

void JSParser::handleIdentifier(v8::Handle<v8::Object> object, bool function)
{
    v8::Handle<v8::String> name = get<v8::String>(object, "name");
    v8::Handle<v8::Array> range = get<v8::Array>(object, "range");
    assert(!range.IsEmpty() && range->Length() == 2);
    const uint32_t offset = get<v8::Integer>(range, 0)->Value();
    const uint32_t length = get<v8::Integer>(range, 1)->Value() - offset;
    CursorInfo c;
    const Location loc(mFileId, offset);
    c.symbolName = String(toCString(name), name->Length());
    c.symbolLength = length;
    error() << "Got function" << c.symbolName << function;
    if (function) {
        c.kind = CursorInfo::JSFunction;
    } else {
        c.kind = CursorInfo::JSVariable;
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
    // error() << "adding" << c << "at" << offset << "scope" << mScope.last().keys();
    for (int i=mParents.size(); i>=0; --i) {
        if (i < mParents.size()) {
            c.symbolName.prepend(mParents.at(i) + '.');
        } else if (c.kind != CursorInfo::JSReference) {
            mScope.last()[c.symbolName] = offset;
        }
        if (c.kind != CursorInfo::JSReference) {
            if (mSymbolNames)
                (*mSymbolNames)[c.symbolName].insert(loc);
        }
    }
}

bool JSParser::recurseObject(v8::Handle<v8::Object> object, const char *name)
{
    if (object.IsEmpty())
        return false;
    bool popScope = false;
    if (name && !strcmp(name, "body")) {
        popScope = true;
        mScope.append(Map<String, uint32_t>());
    }
    if (object->IsArray()) {
        v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(object);
        for (unsigned i=0; i<array->Length(); ++i) {
            recurseObject(get<v8::Object>(array, i), 0);
        }
    } else if (object->IsObject()) {
        v8::Handle<v8::Array> props = object->GetOwnPropertyNames();
        assert(!props.IsEmpty());
        for (unsigned i=0; i<props->Length(); ++i) {
            v8::Handle<v8::String> prop = get<v8::String>(props, i);
            v8::Handle<v8::Object> sub = get<v8::Object>(object, prop);
            if (!sub.IsEmpty() && sub->IsObject()) {
                if (get<v8::String>(sub, "type") == "Identifier") {
                    handleIdentifier(sub, get<v8::String>(object, "type") == "FunctionDeclaration");
                } else {
                    recurseObject(sub, toCString(prop));
                }
            }
        }
    }
    if (popScope)
        mScope.removeLast();

    return true;
}
