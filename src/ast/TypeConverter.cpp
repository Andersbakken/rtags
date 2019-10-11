#include "TypeConverter.h"

namespace
{
const int kPerIsolateDataIndex = 2;
}

struct V8PerIsolateData::Impl {
    std::unordered_map<std::string, v8::Eternal<v8::FunctionTemplate>> m_cstorCache;
};

// static
void V8PerIsolateData::Init(v8::Isolate *isolate)
{
    isolate->SetData(kPerIsolateDataIndex, new Impl());
}

// static
void V8PerIsolateData::Dispose(v8::Isolate *isolate)
{
    delete static_cast<Impl *>(isolate->GetData(kPerIsolateDataIndex));
}

// static
v8::Eternal<v8::FunctionTemplate> &V8PerIsolateData::CstorCache(v8::Isolate *isolate, const char *name)
{
    return (static_cast<Impl *>(isolate->GetData(kPerIsolateDataIndex))->m_cstorCache)[name];
}

// static
v8::Local<v8::String> V8PerIsolateData::String(v8::Isolate *isolate, const char *identifier)
{
    v8::Local<v8::String> value;
    if (!identifier || identifier[0] == '\0') {
        value = v8::String::Empty(isolate);
    } else {
        value = v8::String::NewFromOneByte(
            isolate, reinterpret_cast<const uint8_t *>(identifier), v8::NewStringType::kInternalized, static_cast<int>(strlen(identifier)))
        .ToLocalChecked();
    }
    return value;
}

// static
void V8PerIsolateData::Breakpoint(v8::Isolate *isolate, const char *source)
{
    v8::Local<v8::Context> context = isolate->GetCurrentContext();
    v8::TryCatch tryCatch(isolate);
    std::string sourceStr = std::string("((v) => {debugger})\n//# sourceURL=") + source + ".js";
    fprintf(stderr, "%s\n", sourceStr.c_str());
    v8::Local<v8::Script> script = v8::Script::Compile(context, createV8String(isolate, sourceStr.c_str())).ToLocalChecked();
    v8::Local<v8::Function> f = script->Run(context).ToLocalChecked().As<v8::Function>();
    f->Call(context, context->Global(), 0, nullptr).ToLocalChecked();
}

v8::Local<v8::String> createV8String(v8::Isolate *iso, const char *utf8)
{
    return v8::String::NewFromUtf8(iso, utf8)
#if (V8_MAJOR_VERSION == 7 && V8_MINOR_VERSION >= 9) || V8_MAJOR_VERSION >= 8
    .ToLocalChecked()
#endif
    ;
}

v8::Local<v8::String> createV8String(v8::Isolate *iso, const std::string &utf8)
{
    return createV8String(iso, utf8.c_str());
}


namespace TypeConverter {
bool toV8(v8::Isolate* isolate, const Empty&, v8::Local<v8::Value>* result) {
    *result = v8::Undefined(isolate);
    return true;
}

bool toImpl(v8::Local<v8::Context>, v8::Local<v8::Value>, Empty*) {
    return true;
}

bool toV8(v8::Isolate* isolate, const bool& value, v8::Local<v8::Value>* result) {
    *result = v8::Boolean::New(isolate, value);
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, bool* result) {
    if (!value->IsBoolean())
        return false;
    *result = value.As<v8::Boolean>()->Value();
    return true;
}

bool toV8(v8::Isolate* isolate, const short& value, v8::Local<v8::Value>* result) {
    *result = v8::Integer::New(isolate, value);
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, short* result) {
    if (!value->IsInt32())
        return false;
    *result = static_cast<short>(value.As<v8::Int32>()->Value());
    return true;
}

bool toV8(v8::Isolate* isolate, const int& value, v8::Local<v8::Value>* result) {
    *result = v8::Integer::New(isolate, value);
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, int* result) {
    if (!value->IsInt32())
        return false;
    *result = static_cast<int>(value.As<v8::Int32>()->Value());
    return true;
}

bool toV8(v8::Isolate* isolate, const size_t& value, v8::Local<v8::Value>* result) {
    *result = v8::Integer::New(isolate, value);
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, size_t* result) {
    if (!value->IsUint32())
        return false;
    *result = static_cast<size_t>(value.As<v8::Uint32>()->Value());
    return true;
}

bool toV8(v8::Isolate* isolate, const uint32_t& value, v8::Local<v8::Value>* result) {
    *result = v8::Integer::New(isolate, value);
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, uint32_t* result) {
    if (!value->IsUint32())
        return false;
    *result = static_cast<uint32_t>(value.As<v8::Uint32>()->Value());
    return true;
}

bool toV8(v8::Isolate* isolate, const long long& value, v8::Local<v8::Value>* result) {
    *result = v8::Number::New(isolate, value);
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, long long* result) {
    if (!value->IsNumber())
        return false;
    *result = static_cast<long long>(value.As<v8::Number>()->Value());
    return true;
}

bool toV8(v8::Isolate* isolate, const float& value, v8::Local<v8::Value>* result) {
    *result = v8::Number::New(isolate, value);
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, float* result) {
    if (!value->IsNumber())
        return false;
    *result = static_cast<float>(value.As<v8::Number>()->Value());
    return true;
}

bool toV8(v8::Isolate* isolate, const double& value, v8::Local<v8::Value>* result) {
    *result = v8::Number::New(isolate, value);
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, double* result) {
    if (!value->IsNumber())
        return false;
    *result = value.As<v8::Number>()->Value();
    return true;
}

bool toV8(v8::Isolate* isolate, const char* str, v8::Local<v8::Value>* result) {
    // TODO: should never be nullptr
    if (!str)
        return v8::String::NewFromUtf8(isolate, "", v8::NewStringType::kNormal).ToLocal(result);
    return v8::String::NewFromUtf8(isolate, str, v8::NewStringType::kNormal).ToLocal(result);
}

bool toV8(v8::Isolate* isolate, const std::string& value, v8::Local<v8::Value>* result) {
    // TODO: we need to check that std::string is always Utf8String.
    return v8::String::NewFromUtf8(isolate, value.c_str(), v8::NewStringType::kNormal, value.size()).ToLocal(result);
}

bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::string* result) {
    if (!value->IsString())
        return false;
    // TODO: we need to check that std::string is always Utf8String.
    v8::Local<v8::String> stringValue = value.As<v8::String>();
    result->resize(stringValue->Utf8Length(context->GetIsolate()));
    stringValue->WriteUtf8(context->GetIsolate(), &(*result)[0]);
    return true;
}

bool toV8(v8::Isolate* isolate, const unsigned long long& value, v8::Local<v8::Value>* result) {
    *result = v8::Number::New(isolate, static_cast<double>(value));
    return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, unsigned long long* result) {
    if (value->IsInt32() || value->IsInt32())
        *result = static_cast<unsigned long long>(value.As<v8::Integer>()->Value());
    else if (value->IsBigInt())
        *result = static_cast<unsigned long long>(value.As<v8::BigInt>()->Uint64Value());
    else if (value->IsNumber())
        *result = static_cast<unsigned long long>(value.As<v8::Number>()->Value());
    else
        return false;
    return true;
}

// bool toV8(v8::Isolate* isolate, const MemoryBuffer& buffer, v8::Local<v8::Value>* result) {
//     v8::Local<v8::ArrayBuffer> arrayBuffer = v8::ArrayBuffer::New(isolate, buffer.getStore());
//     if (buffer.getType() == MemoryBufferType::kArrayBuffer)
//         *result = arrayBuffer;
//     else if (buffer.getType() == MemoryBufferType::kUint8Array)
//         *result = v8::Uint8Array::New(arrayBuffer, buffer.getByteOffset(), buffer.getByteLength());
//     else
//         return false;
//     return true;
// }

// bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, MemoryBuffer* view) {
//     if (value->IsArrayBuffer()) {
//         v8::Local<v8::ArrayBuffer> arrayBuffer = value.As<v8::ArrayBuffer>();
//         *view = MemoryBuffer(arrayBuffer->GetBackingStore(), 0, arrayBuffer->ByteLength(), MemoryBufferType::kArrayBuffer);
//         return true;
//     } else if (value->IsUint8Array()) {
//         v8::Local<v8::Uint8Array> uint8Array = value.As<v8::Uint8Array>();
//         v8::Local<v8::ArrayBuffer> arrayBuffer = uint8Array->Buffer();
//         *view = MemoryBuffer(arrayBuffer->GetBackingStore(), uint8Array->ByteOffset(), uint8Array->ByteLength(), MemoryBufferType::kUint8Array);
//         return true;
//     }
//     return false;
// }

template<typename T>
bool toV8(v8::Isolate* isolate, const std::vector<T>& values, v8::Local<v8::Value>* result) {
    std::vector<v8::Local<v8::Value>> resultValues(values.size());
    for (size_t i = 0; i < values.size(); ++i) {
        if (!toV8(isolate, values[i], &resultValues[i]))
            return false;
    }
    *result = v8::Array::New(isolate, resultValues.data(), resultValues.size());
    return true;
}

template<typename T>
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<T>* result) {
    if (!value->IsArray())
        return false;
    v8::Local<v8::Array> arrayValue = value.As<v8::Array>();
    result->resize(arrayValue->Length());
    for (uint32_t i = 0; i < arrayValue->Length(); ++i) {
        v8::Local<v8::Value> v;
        if (!arrayValue->Get(context, i).ToLocal(&v))
            return false;
        if (!toImpl(context, v, &(*result)[i]))
            return false;
    }
    return true;
}

template<typename T>
bool toV8(v8::Isolate* isolate, const std::vector<std::pair<std::string, T>>& values, v8::Local<v8::Value>* result) {
    std::vector<v8::Local<v8::Name>> keyValues(values.size());
    std::vector<v8::Local<v8::Value>> valueValues(values.size());
    for (size_t i = 0; i < values.size(); ++i) {
        if (!toV8(isolate, values[i].first, reinterpret_cast<v8::Local<v8::Value>*>(&keyValues[i])))
            return false;
        if (!toV8(isolate, values[i].second, &valueValues[i]))
            return false;
    }
    *result = v8::Object::New(isolate, v8::Null(isolate), &keyValues[0], &valueValues[0], keyValues.size());
    return true;
}

template<typename T>
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<std::pair<std::string, T>>* result) {
    if (!value->IsObject())
        return false;
    v8::Local<v8::Object> objectValue = value.As<v8::Object>();
    v8::Local<v8::Array> propertyNames;
    if (!objectValue->GetOwnPropertyNames(context).ToLocal(&propertyNames))
        return false;
    result->resize(propertyNames->Length());
    for (uint32_t i = 0; i < propertyNames->Length(); ++i) {
        v8::Local<v8::Value> keyValue;
        if (!propertyNames->Get(context, i).ToLocal(&keyValue))
            return false;
        v8::Local<v8::Value> valueValue;
        if (!objectValue->Get(context, keyValue).ToLocal(&valueValue))
            return false;
        if (!toImpl(context, keyValue, &(*result)[i].first))
            return false;
        if (!toImpl(context, valueValue, &(*result)[i].second))
            return false;
    }
    return true;
}

template<typename T>
bool toV8(v8::Isolate* isolate, const Optional<T>& value, v8::Local<v8::Value>* result) {
    if (!value) {
        *result = v8::Undefined(isolate);
        return true;
    }
    return toV8(isolate, *value, result);
}

template<typename T>
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<T>* result) {
    if (value->IsNullOrUndefined()) {
        *result = Optional<T>();
        return true;
    }
    T resultValue;
    if (toImpl(context, value, &resultValue)) {
        *result = resultValue;
        return true;
    }
    return false;
}

template bool toV8(v8::Isolate* isolate, const std::vector<int>& value, v8::Local<v8::Value>* result);
template bool toV8(v8::Isolate* isolate, const std::vector<uint32_t>& value, v8::Local<v8::Value>* result);
template bool toV8(v8::Isolate* isolate, const std::vector<std::string>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<int>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<std::string>* result);
template bool toV8(v8::Isolate* isolate, const std::vector<std::pair<std::string,int>>& value, v8::Local<v8::Value>* result);
template bool toV8(v8::Isolate* isolate, const std::vector<std::pair<std::string,bool>>& value, v8::Local<v8::Value>* result);
template bool toV8(v8::Isolate* isolate, const std::vector<std::pair<std::string,std::string>>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<std::pair<std::string,int>>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<std::pair<std::string,std::string>>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<std::vector<std::pair<std::string,std::string>>>* result);
template bool toV8(v8::Isolate* isolate, const Optional<std::vector<std::pair<std::string,std::string>>>& value, v8::Local<v8::Value>* result);
template bool toV8(v8::Isolate* isolate, const Optional<bool>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<bool>* result);
template bool toV8(v8::Isolate* isolate, const Optional<int>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<int>* result);
template bool toV8(v8::Isolate* isolate, const Optional<short>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<short>* result);
template bool toV8(v8::Isolate* isolate, const Optional<std::string>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<std::string>* result);
template bool toV8(v8::Isolate* isolate, const Optional<float>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<float>* result);
template bool toV8(v8::Isolate* isolate, const Optional<long long>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<long long>* result);
template bool toV8(v8::Isolate* isolate, const Optional<std::vector<std::string>>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<std::vector<std::string>>* result);
template bool toV8(v8::Isolate* isolate, const Optional<std::vector<Optional<std::string>>>& value, v8::Local<v8::Value>* result);
template bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<std::vector<Optional<std::string>>>* result);
} // namespace TypeConverter

// home; tree (not elka or palma); human; unexistent animal
