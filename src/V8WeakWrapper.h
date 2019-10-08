#ifndef SRC_NBP_V8_BRIDGE_V8_WEAK_WRAPPER_H_
#define SRC_NBP_V8_BRIDGE_V8_WEAK_WRAPPER_H_

#include <v8.h>
// TODO: move this one to V8Utils and share
namespace bridge {
template<typename T>
class V8WeakWrapper {
public:
  typedef void (*Callback)(v8::Isolate* isolate);
  V8WeakWrapper(v8::Isolate* isolate, v8::Local<v8::Object> object, std::shared_ptr<T> value, Callback callback = nullptr)
    : m_value(std::move(value))
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
} // namespace bridge

#endif // SRC_NBP_V8_BRIDGE_V8_WEAK_WRAPPER_H_
