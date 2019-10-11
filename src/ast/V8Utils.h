#ifndef TYPE_CONVERTER_H
#define TYPE_CONVERTER_H

#include <string>
#include <vector>
#include <v8.h>

struct Empty {};

template<typename T>
class V8WeakWrapper {
public:
    typedef void (*Callback)(v8::Isolate* isolate);
    V8WeakWrapper(v8::Isolate* isolate, v8::Local<v8::Object> object, const std::shared_ptr<T> &value, Callback callback = nullptr)
        : m_value(value)
        , m_isolate(isolate)
        , m_holder(isolate, object)
        , m_callback(callback) {
        object->SetAlignedPointerInInternalField(1, this);
        m_holder.SetWeak(this, weakCallback, v8::WeakCallbackType::kInternalFields);
    }
    ~V8WeakWrapper() {
        if (m_callback)
            m_callback(m_isolate);
    }

    static std::shared_ptr<T> unwrap(v8::Local<v8::Object> object) {
        if (object->InternalFieldCount() != 2)
            return nullptr;
        return static_cast<V8WeakWrapper<T>*>(object->GetAlignedPointerFromInternalField(1))->m_value;
    }

private:
    static void weakCallback(const v8::WeakCallbackInfo<V8WeakWrapper>& data) {
        delete static_cast<V8WeakWrapper*>(data.GetInternalField(1));
    }

    std::shared_ptr<T> m_value;
    v8::Isolate* m_isolate;
    v8::Global<v8::Object> m_holder;
    Callback m_callback;
};

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

v8::Local<v8::String> createV8String(v8::Isolate *iso, const char *utf8);
v8::Local<v8::String> createV8String(v8::Isolate *iso, const std::string &utf8);

class MemoryBuffer;
template <typename T> class Optional
{
public:
    Optional()
        : hasValue(false)
    {
    }
    Optional(T v)
        : hasValue(true), value(std::move(v))
    {
    }
    explicit operator bool() const
    {
        return hasValue;
    }
    const T &operator*() const
    {
        return value;
    }
    const T &get() const
    {
        return value;
    }

private:
    bool hasValue;
    T value;
};

namespace TypeConverter
{
bool toV8(v8::Isolate *isolate, const Empty &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> result, Empty *value);

bool toV8(v8::Isolate *isolate, const char *str, v8::Local<v8::Value> *result);

bool toV8(v8::Isolate *isolate, const bool &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, bool *result);

bool toV8(v8::Isolate *isolate, const short &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, short *result);

bool toV8(v8::Isolate *isolate, const int &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, int *result);

bool toV8(v8::Isolate *isolate, const size_t &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, size_t *result);

bool toV8(v8::Isolate *isolate, const uint32_t &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, uint32_t *result);

bool toV8(v8::Isolate *isolate, const long long &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, long long *result);

bool toV8(v8::Isolate *isolate, const float &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, float *result);

bool toV8(v8::Isolate *isolate, const double &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, double *result);

bool toV8(v8::Isolate *isolate, const std::string &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::string *result);

bool toV8(v8::Isolate *isolate, const unsigned long long &value, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, unsigned long long *result);

bool toV8(v8::Isolate *isolate, const MemoryBuffer &buffer, v8::Local<v8::Value> *result);
bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, MemoryBuffer *view);

template<typename T> bool toV8(v8::Isolate* isolate, const Optional<T>& value, v8::Local<v8::Value>* result);
template<typename T> bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, Optional<T>* result);

// template<typename T> bool toV8(v8::Isolate* isolate, const std::vector<T>& values, v8::Local<v8::Value>* result);
// template<typename T> bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<T>* result);

// template<typename T> bool toV8(v8::Isolate* isolate, const std::vector<std::pair<std::string, T>>& values, v8::Local<v8::Value>* result);
// template<typename T> bool toImpl(v8::Local<v8::Context> context, v8::Local<v8::Value> value, std::vector<std::pair<std::string, T>>* result);
}

#endif
