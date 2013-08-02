#include "JSONParser.h"
#include <rct/Log.h>
#include <v8.h>
#include <assert.h>

static Value v8ValueToValue(v8::Handle<v8::Value> v8Value)
{
    v8::HandleScope scope;
    if (v8Value->IsArray()) {
        List<Value> value;
        const v8::Handle<v8::Array> v8Array = v8::Handle<v8::Array>::Cast(v8Value);
        const uint32_t size = v8Array->Length();
        for (uint32_t i = 0; i < size; ++i) {
            if (v8Array->Has(i))
                value.append(v8ValueToValue(v8Array->Get(i)));
            else
                value.append(Value());
        }
        return Value(value);
    } else if (v8Value->IsObject()) {
        Map<String, Value> value;
        const v8::Handle<v8::Object> v8Object = v8Value->ToObject();
        const v8::Handle<v8::Array> props = v8Object->GetPropertyNames();
        const uint32_t size = props->Length();
        for (uint32_t i = 0; i < size; ++i) {
            assert(props->Has(i));
            const v8::Handle<v8::Value> name = props->Get(i);
            value[String(*v8::String::Utf8Value(name))] = v8ValueToValue(v8Object->Get(name));
        }
        return Value(value);
    } else if (v8Value->IsBoolean()) {
        return Value(v8Value->BooleanValue());
    } else if (v8Value->IsInt32() || v8Value->IsUint32()) {
        return Value(v8Value->Int32Value());
    } else if (v8Value->IsNumber()) {
        return Value(v8Value->NumberValue());
    } else if (v8Value->IsString()) {
        return Value(String(*v8::String::Utf8Value(v8Value)));
    } else {
        error() << "Unexpected v8 value type in JSONParser";
    }
    // undefined or null?
    return Value();
}

bool JSONParser::parse(const String& json)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    const v8::Isolate::Scope isolateScope(isolate);
    v8::HandleScope handleScope;

    v8::Handle<v8::ObjectTemplate> globalTemplate = v8::ObjectTemplate::New();
    v8::Handle<v8::Context> context = v8::Context::New(0, globalTemplate);
    v8::Context::Scope contextScope(context);

    v8::Handle<v8::Object> global = context->Global();

    v8::Handle<v8::Object> JSON = global->Get(v8::String::New("JSON"))->ToObject();
    v8::Handle<v8::Function> JSON_parse = v8::Handle<v8::Function>::Cast(JSON->Get(v8::String::New("parse")));

    v8::Handle<v8::Value> data = v8::String::New(json.constData(), json.size());
    v8::Handle<v8::Value> value = JSON_parse->Call(JSON, 1, &data);

    mRoot = v8ValueToValue(value);
    return isValid();
}
