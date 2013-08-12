#include "JSParser.h"
#include <rct/RegExp.h>

#define toCString(str) *v8::String::Utf8Value(str)

template <typename T>
static v8::Handle<v8::String> toJSON(v8::Handle<T> obj, bool pretty)
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

    v8::Handle<v8::Value> ret = JSON_stringify->Call(JSON, pretty ? 3 : 2, args);
    if (!ret.IsEmpty())
        return scope.Close(ret)->ToString();
    return v8::String::New("can't json this");
}

Log operator<<(Log log, v8::Handle<v8::String> string)
{
    if (!string.IsEmpty())
        log << toCString(string);
    return log;
}

Log operator<<(Log log, v8::Handle<v8::Value> value)
{
    if (!value.IsEmpty())
        log << toCString(toJSON(value, true));
    return log;
}

Log operator<<(Log log, v8::Handle<v8::Object> value)
{
    if (!value.IsEmpty())
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

JSParser::JSParser()
    : mIsolate(0)
{}

JSParser::~JSParser()
{
    {
        const v8::Isolate::Scope isolateScope(mIsolate);
#ifdef V8_DISPOSE_HAS_ISOLATE
        if (!mParse.IsEmpty())
            mParse.Dispose(mIsolate);
        if (!mContext.IsEmpty())
            mContext.Dispose(mIsolate);
#else
        if (!mParse.IsEmpty())
            mParse.Dispose();
        if (!mContext.IsEmpty())
            mContext.Dispose();
#endif
    }
    mIsolate->Dispose();
}

// v8::Handle<v8::Value> Print(const v8::Arguments& args);
v8::Handle<v8::Value> log(const v8::Arguments &args)
{
    Log out(Error);
    const int length = args.Length();
    for (int i=0; i<length; ++i) {
        out << args[i];
    }
    return v8::Undefined();
}

// v8::Handle<v8::Value> Print(const v8::Arguments& args);
v8::Handle<v8::Value> jsDefine(const v8::Arguments &args)
{
    error() << "got define called" << args[0];
    Log out(Error);
    const int length = args.Length();
    for (int i=0; i<length; ++i) {
        out << args[i];
    }
    return v8::Undefined();
}


bool JSParser::init()
{
    mIsolate = v8::Isolate::GetCurrent();
    const v8::Isolate::Scope isolateScope(mIsolate);
    v8::HandleScope handleScope;
    v8::Handle<v8::ObjectTemplate> globalObjectTemplate = v8::ObjectTemplate::New();
    globalObjectTemplate->Set(v8::String::New("log"), v8::FunctionTemplate::New(log));

    v8::Handle<v8::Context> ctx = v8::Context::New(mIsolate, 0, globalObjectTemplate);
    mContext.Reset(mIsolate, ctx);
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
        printf("[%s:%d]: return false;\n", __func__, __LINE__); fflush(stdout);
        return false;
    }
    script->Run();

    v8::Handle<v8::Object> global = mContext->Global();
    mParse = getPersistent<v8::Function>(global, "indexFile");

    return !mParse.IsEmpty() && mParse->IsFunction();
}

bool JSParser::parse(const Path &path, SymbolMap *symbols, SymbolNameMap *symbolNames, String *ast)
{
    String contents = path.readAll();
    if (contents.isEmpty()) {
        error() << "No contents for" << path;
        return false;
    }
    const int initialSize = contents.size();

    const RegExp rx("^// *include( *\"\\([^\"]\\+\\)\" *)");
    List<RegExp::Capture> caps;
    List<Path> includes;

    int idx = -1;
    while ((idx = rx.indexIn(contents, idx + 1, &caps)) != -1) {
        assert(caps.size() == 2);
        Path p = caps.at(1).capture;
        if (p.isFile()) {
            includes.append(p);
        }
    }
    for (List<Path>::const_iterator it = includes.begin(); it != includes.end(); ++it) {
        const String c = it->readAll();
        contents.insert(0, c);
    }
    const uint32_t limit = contents.size() - initialSize;

    const v8::Isolate::Scope isolateScope(mIsolate);
    // mFileId = Location::insertFile(path);
    v8::HandleScope handleScope;
    v8::Context::Scope scope(mContext);
    v8::Handle<v8::Value> args[3];
    args[0] = v8::String::New(contents.constData(), contents.size());
    args[1] = v8::String::New(path.constData(), path.size());
    args[2] = v8::Boolean::New(ast);

    assert(!mParse.IsEmpty() && mParse->IsFunction());
    assert(!args[0].IsEmpty() && args[0]->IsString());
    assert(!args[1].IsEmpty() && args[1]->IsString());
    assert(!args[2].IsEmpty() && args[2]->IsBoolean());
    v8::Handle<v8::Value> resultVal = mParse->Call(mContext->Global(), 3, args);
    if (resultVal.IsEmpty() || !resultVal->IsObject())
        return false;
    v8::Handle<v8::Object> result = v8::Handle<v8::Object>::Cast(resultVal);
    if (ast) {
        v8::Handle<v8::Object> astObject = get<v8::Object>(result, "ast");
        if (!astObject.IsEmpty())
            *ast = toCString(toJSON(astObject, true));
    }
    // error() << result;
    v8::Handle<v8::Array> res = get<v8::Array>(result, "objects");
    if (!res.IsEmpty()) {
        const uint32_t fileId = Location::insertFile(path);
        for (unsigned i=0; i<res->Length(); ++i) {
            v8::Handle<v8::Object> scope = get<v8::Object>(res, i);
            assert(!scope.IsEmpty());
            const v8::Handle<v8::Array> props = scope->GetOwnPropertyNames();
            const int propCount = props->Length();
            // error() << "Got a scope" << scope;
            for (int j=0; j<propCount; ++j) {
                const v8::Handle<v8::String> key = get<v8::String>(props, j);
                const v8::Handle<v8::Array> refs = get<v8::Array>(scope, key);
                const int refCount = refs->Length();
                String keyString = toCString(key);
                CursorInfo *decl = 0;
                Map<Location, CursorInfo*> pendingRefCursors;
                Location declLoc;
                for (int k=0; k<refCount; ++k) {
                    const v8::Handle<v8::Array> ref = get<v8::Array>(refs, k);
                    const uint32_t off = static_cast<uint32_t>(get<v8::Number>(ref, 0)->Value());
                    const Location loc(fileId, off);
                    CursorInfo &c = (*symbols)[loc];
                    c.start = loc.offset();
                    c.end = static_cast<uint32_t>(get<v8::Number>(ref, 1)->Value());
                    c.symbolLength = c.end - c.start;
                    c.symbolName = keyString;
                    if (ref->Length() == 3) {
                        (*symbolNames)[keyString].insert(loc);
                        c.kind = CursorInfo::JSDeclaration;
                        decl = &c;
                        declLoc = loc;
                        for (Map<Location, CursorInfo*>::const_iterator it = pendingRefCursors.begin(); it != pendingRefCursors.end(); ++it) {
                            it->second->targets.insert(declLoc);
                            c.references.insert(it->first);
                        }
                        // error() << "Got a declaration" << loc << keyString << pendingRefCursors.size();
                        pendingRefCursors.clear();
                    } else {
                        c.kind = CursorInfo::JSReference;
                        if (decl) {
                            decl->references.insert(loc);
                            c.targets.insert(declLoc);
                        } else {
                            pendingRefCursors[loc] = &c;
                        }
                        // error() << "Got a reference" << loc << keyString;
                    }
                }
            }
        }
    }

    v8::Handle<v8::Array> errors = get<v8::Array>(result, "errors");
    if (!errors.IsEmpty() && errors->Length() && testLog(RTags::CompilationErrorXml)) {
        const int length = errors->Length();
        log(RTags::CompilationErrorXml, "<?xml version=\"1.0\" encoding=\"utf-8\"?><checkstyle><file name=\"%s\">", path.constData());
        for (int i=0; i<length; ++i) {
            v8::Handle<v8::Object> error = get<v8::Array>(errors, i);
            assert(!error.IsEmpty());
            log(RTags::CompilationErrorXml, "<error line=\"%d\" column=\"%d\" startOffset=\"%d\" severity=\"error\" message=\"%s\"/>",
                static_cast<int>(get<v8::Number>(error, "lineNumber")->Value()),
                static_cast<int>(get<v8::Number>(error, "column")->Value()),
                static_cast<int>(get<v8::Number>(error, "index")->Value()),
                toCString(get<v8::String>(error, "description")));
        }

        logDirect(RTags::CompilationErrorXml, "</file></checkstyle>");
    }

    return true;
}

