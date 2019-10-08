/* (c) 2019 Netflix, Inc. Do not copy or use without prior written permission from Netflix, Inc. */

#include "V8SourceLocation.h"

#include <v8.h>

#include "AST.h"

#include "V8Utils.h"

#include "TypeConverter.h"

namespace bridge {

namespace TypeConverter {
bool toV8(v8::Isolate* isolate, const std::shared_ptr<AST::SourceLocation>& value, v8::Local<v8::Value>* result) {
  if (!value) {
    *result = v8::Null(isolate);
    return false;
  }
  v8::Local<v8::Object> object;
  if (!V8SourceLocation::wrap(isolate, isolate->GetCurrentContext(), value.get()).ToLocal(&object))
    return false;
  new V8WeakWrapper<AST::SourceLocation>(isolate, object, value);
  *result = object;
  return true;
}

bool toImpl(v8::Local<v8::Context> /* context */, v8::Local<v8::Value> value, std::shared_ptr<AST::SourceLocation>* result) {
  if (value->IsNullOrUndefined())
    return true;
  if (!value->IsObject())
    return false;
  *result = V8WeakWrapper<AST::SourceLocation>::unwrap(v8::Local<v8::Object>::Cast(value));
  return !!*result;
}
} // namespace TypeConverter

// static
v8::MaybeLocal<v8::Object> V8SourceLocation::wrap(v8::Isolate* isolate, v8::Local<v8::Context> context, AST::SourceLocation* impl) {
  v8::Local<v8::Function> cstor;
  if (constructorTemplate(isolate)->GetFunction(context).ToLocal(&cstor)) {
    v8::Local<v8::Object> instance;
    if (cstor->NewInstance(context).ToLocal(&instance)) {
      int index = 0;
      void* values[] = {impl};
      instance->SetAlignedPointerInInternalFields(1, &index, values);
      return instance;
    }
  }
  return v8::MaybeLocal<v8::Object>();
}

// static
AST::SourceLocation* V8SourceLocation::impl(v8::Local<v8::Object> object) {
  if (object->InternalFieldCount() < 1)
    return nullptr;
  return static_cast<AST::SourceLocation*>(object->GetAlignedPointerFromInternalField(0));
}

namespace {
void cstorCallback(const v8::FunctionCallbackInfo<v8::Value>& info) {
  int index = 0;
  void* values[] = {nullptr};
  info.This()->SetAlignedPointerInInternalFields(1, &index, values);
}
}

// static
v8::Local<v8::FunctionTemplate> V8SourceLocation::constructorTemplate(v8::Isolate* isolate) {
  auto& cacheValue = V8PerIsolateData::CstorCache(isolate, "AST::SourceLocation");
  if (!cacheValue.IsEmpty())
    return cacheValue.Get(isolate);
  v8::Local<v8::FunctionTemplate> cstorTemplate = v8::FunctionTemplate::New(isolate, cstorCallback);
  cstorTemplate->SetClassName(V8PerIsolateData::String(isolate, "AST::SourceLocation"));
  v8::Local<v8::ObjectTemplate> templ = cstorTemplate->InstanceTemplate();
  v8::Local<v8::ObjectTemplate> prototypeTempl = cstorTemplate->PrototypeTemplate();
  templ->SetInternalFieldCount(2);

  prototypeTempl->Set(
      V8PerIsolateData::String(isolate, "toString"),
      v8::FunctionTemplate::New(isolate, toStringCallback));
  prototypeTempl->SetAccessorProperty(
      V8PerIsolateData::String(isolate, "line"),
      v8::FunctionTemplate::New(isolate, lineGetterCallback));
  prototypeTempl->SetAccessorProperty(
      V8PerIsolateData::String(isolate, "column"),
      v8::FunctionTemplate::New(isolate, columnGetterCallback));
  prototypeTempl->SetAccessorProperty(
      V8PerIsolateData::String(isolate, "offset"),
      v8::FunctionTemplate::New(isolate, offsetGetterCallback));
  prototypeTempl->SetAccessorProperty(
      V8PerIsolateData::String(isolate, "file"),
      v8::FunctionTemplate::New(isolate, fileGetterCallback));

  cacheValue.Set(isolate, cstorTemplate);
  return cstorTemplate;
}


// static
void V8SourceLocation::toStringCallback(const v8::FunctionCallbackInfo<v8::Value>& info) {
  v8::Isolate* isolate = info.GetIsolate();
  AST::SourceLocation* i = impl(info.This());
  if (!i) {
    info.GetIsolate()->ThrowException(v8::String::NewFromUtf8(isolate, "AST::SourceLocation::toString").ToLocalChecked());
    return;
  }

  v8::Local<v8::Value> value;
  if (TypeConverter::toV8(info.GetIsolate(), i->toString(), &value))
    info.GetReturnValue().Set(value);
}
//static
void V8SourceLocation::lineGetterCallback(const v8::FunctionCallbackInfo<v8::Value>& info) {
  AST::SourceLocation* i = impl(info.This());
  if (!i) {
    // info.GetIsolate()->ThrowException(v8::String::NewFromUtf8(info.GetIsolate(), "AST::SourceLocation::line").ToLocalChecked());
    return;
  }
  v8::Local<v8::Value> value;
  if (TypeConverter::toV8(info.GetIsolate(), i->line(), &value))
    info.GetReturnValue().Set(value);
}
//static
void V8SourceLocation::columnGetterCallback(const v8::FunctionCallbackInfo<v8::Value>& info) {
  AST::SourceLocation* i = impl(info.This());
  if (!i) {
    // info.GetIsolate()->ThrowException(v8::String::NewFromUtf8(info.GetIsolate(), "AST::SourceLocation::column").ToLocalChecked());
    return;
  }
  v8::Local<v8::Value> value;
  if (TypeConverter::toV8(info.GetIsolate(), i->column(), &value))
    info.GetReturnValue().Set(value);
}
//static
void V8SourceLocation::offsetGetterCallback(const v8::FunctionCallbackInfo<v8::Value>& info) {
  AST::SourceLocation* i = impl(info.This());
  if (!i) {
    // info.GetIsolate()->ThrowException(v8::String::NewFromUtf8(info.GetIsolate(), "AST::SourceLocation::offset").ToLocalChecked());
    return;
  }
  v8::Local<v8::Value> value;
  if (TypeConverter::toV8(info.GetIsolate(), i->offset(), &value))
    info.GetReturnValue().Set(value);
}
//static
void V8SourceLocation::fileGetterCallback(const v8::FunctionCallbackInfo<v8::Value>& info) {
  AST::SourceLocation* i = impl(info.This());
  if (!i) {
    // info.GetIsolate()->ThrowException(v8::String::NewFromUtf8(info.GetIsolate(), "AST::SourceLocation::file").ToLocalChecked());
    return;
  }
  v8::Local<v8::Value> value;
  if (TypeConverter::toV8(info.GetIsolate(), i->file(), &value))
    info.GetReturnValue().Set(value);
};


// static
void V8SourceLocation::breakpoint(v8::Isolate* /* isolate */, const char* /* source */, v8::Local<v8::Name> /* name */, v8::Local<v8::Value> /* value */) {

}

} // namespace bridge
