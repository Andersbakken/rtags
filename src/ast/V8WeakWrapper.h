#ifndef V8_WEAK_WRAPPER_H_
#define V8_WEAK_WRAPPER_H_

#include <v8.h>
// TODO: move this one to V8Utils and share
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

#endif
