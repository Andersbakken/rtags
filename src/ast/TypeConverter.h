/* (c) 2019 Netflix, Inc. Do not copy or use without prior written permission from Netflix, Inc. */

#ifndef SRC_NRD_NBP_V8_BRIDGE_TYPE_CONVERTER_H_
#define SRC_NRD_NBP_V8_BRIDGE_TYPE_CONVERTER_H_

#include <string>
#include <vector>

namespace netflix {
class DataBuffer;
class Variant;
} // namespace netflix

namespace v8 {
class Isolate;
template<typename Value>
class Local;
class Value;
class Context;
} // namespace v8

class MemoryBuffer;

struct Empty {};

template<typename T>
class Optional {
public:
  Optional() : hasValue(false) {}
  Optional(T v) : hasValue(true), value(std::move(v)) {}

  explicit operator bool() const { return hasValue; }
  const T& operator*() const { return value; }
  const T& get() const { return value; }

private:
  bool hasValue;
  T value;
};

namespace TypeConverter {
bool toV8(v8::Isolate* isolate, const Empty& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> result, Empty* value);

bool toV8(v8::Isolate* isolate, const char* str, v8::Local<v8::Value>* result);

bool toV8(v8::Isolate* isolate, const bool& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, bool* result);

bool toV8(v8::Isolate* isolate, const short& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, short* result);

bool toV8(v8::Isolate* isolate, const int& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, int* result);

bool toV8(v8::Isolate* isolate, const size_t& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, size_t* result);

bool toV8(v8::Isolate* isolate, const uint32_t& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, uint32_t* result);

bool toV8(v8::Isolate* isolate, const long long& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, long long* result);

bool toV8(v8::Isolate* isolate, const float& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, float* result);

bool toV8(v8::Isolate* isolate, const double& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, double* result);

bool toV8(v8::Isolate* isolate, const std::string& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::string* result);

bool toV8(v8::Isolate* isolate, const unsigned long long& value, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, unsigned long long* result);

bool toV8(v8::Isolate* isolate, const MemoryBuffer& buffer, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, MemoryBuffer* view);

bool toV8(v8::Isolate* isolate, const netflix::DataBuffer& buffer, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, netflix::DataBuffer* view);

bool toV8(v8::Isolate* isolate, const netflix::Variant& buffer, v8::Local<v8::Value>* result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, netflix::Variant* view);

template<typename T>
bool toV8(v8::Isolate* isolate, const Optional<T>& value, v8::Local<v8::Value>* result);
template<typename T>
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<T>* result);

template<typename T>
bool toV8(v8::Isolate* isolate, const std::vector<T>& value, v8::Local<v8::Value>* result);
template<typename T>
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<T>* result);
// TODO: should we replace it with netflix::Variant?
template<typename T>
bool toV8(v8::Isolate* isolate, const std::vector<std::pair<std::string, T>>& value, v8::Local<v8::Value>* result);
template<typename T>
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<std::pair<std::string, T>>* result);
} // namespace TypeConverter

#endif // SRC_NRD_NBP_V8_BRIDGE_TYPE_CONVERTER_H_
