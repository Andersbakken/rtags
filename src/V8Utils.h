/* (c) 2019 Netflix, Inc. Do not copy or use without prior written permission from Netflix, Inc. */

#ifndef SRC_NRD_NBP_V8_BRIDGE_V8UTILS_H_
#define SRC_NRD_NBP_V8_BRIDGE_V8UTILS_H_

#include <memory>
#include <vector>

namespace v8 {
template<typename T>
class Eternal;
template<typename T>
class Local;
class Isolate;
class FunctionTemplate;
class String;
} // namespace v8

namespace bridge {
class V8PerIsolateData {
public:
  static void Init(v8::Isolate* isolate);
  static void Dispose(v8::Isolate* isolate);

  static v8::Eternal<v8::FunctionTemplate>& CstorCache(v8::Isolate* isolate, const char* name);
  static v8::Local<v8::String> String(v8::Isolate* isolate, const char* identifier);

  static void Breakpoint(v8::Isolate* isolate, const char* source);

private:
  V8PerIsolateData();

  struct Impl;
  std::unique_ptr<Impl> m_impl;
};
} // namespace bridge

#endif // SRC_NRD_NBP_V8_BRIDGE_V8UTILS_H_
