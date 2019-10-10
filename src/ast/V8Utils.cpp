/* (c) 2019 Netflix, Inc. Do not copy or use without prior written permission from Netflix, Inc. */

#include "V8Utils.h"

#include <v8.h>

#include <string>
#include <unordered_map>

namespace {
const int kPerIsolateDataIndex = 2;
}

struct V8PerIsolateData::Impl {
  std::unordered_map<std::string, v8::Eternal<v8::FunctionTemplate>> m_cstorCache;
};

// static
void V8PerIsolateData::Init(v8::Isolate* isolate) {
  isolate->SetData(kPerIsolateDataIndex, new Impl());
}

// static
void V8PerIsolateData::Dispose(v8::Isolate* isolate) {
  delete static_cast<Impl*>(isolate->GetData(kPerIsolateDataIndex));
}

// static
v8::Eternal<v8::FunctionTemplate>& V8PerIsolateData::CstorCache(v8::Isolate* isolate, const char* name) {
  return (static_cast<Impl*>(isolate->GetData(kPerIsolateDataIndex))->m_cstorCache)[name];
}

// static
v8::Local<v8::String> V8PerIsolateData::String(v8::Isolate* isolate, const char* identifier) {
  v8::Local<v8::String> value;
  if (!identifier || identifier[0] == '\0') {
    value = v8::String::Empty(isolate);
  } else {
    value = v8::String::NewFromOneByte(
                isolate, reinterpret_cast<const uint8_t*>(identifier),
                v8::NewStringType::kInternalized, static_cast<int>(strlen(identifier))).ToLocalChecked();
  }
  return value;
}

// static
void V8PerIsolateData::Breakpoint(v8::Isolate* isolate, const char* source) {
  v8::Local<v8::Context> context = isolate->GetCurrentContext();
  v8::TryCatch tryCatch(isolate);
  std::string sourceStr = std::string("((v) => {debugger})\n//# sourceURL=") + source + ".js";
  fprintf(stderr, "%s\n", sourceStr.c_str());
  v8::Local<v8::Script> script = v8::Script::Compile(context, v8::String::NewFromUtf8(isolate, sourceStr.c_str()).ToLocalChecked()).ToLocalChecked();
  v8::Local<v8::Function> f = script->Run(context).ToLocalChecked().As<v8::Function>();
  f->Call(context, context->Global(), 0, nullptr).ToLocalChecked();
}
