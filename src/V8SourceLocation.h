/* (c) 2019 Netflix, Inc. Do not copy or use without prior written permission from Netflix, Inc. */
#ifndef SRC_NRD_NBP_V8_BRIDGE_V8_SOURCELOCATION_H_
#define SRC_NRD_NBP_V8_BRIDGE_V8_SOURCELOCATION_H_

#include <memory>

#include "V8Forward.h"
#include "V8WeakWrapper.h"
#include "AST.h"

namespace bridge
{
namespace TypeConverter
{
bool toV8(v8::Isolate *isolate, const std::shared_ptr<AST::SourceLocation> &value, v8::Local<v8::Value> *);
bool toImpl(v8::Local<v8::Context>, v8::Local<v8::Value> value, std::shared_ptr<AST::SourceLocation> *result);
} // namespace TypeConverter

class V8SourceLocation
{
public:
    static v8::MaybeLocal<v8::Object> wrap(v8::Isolate *isolate, v8::Local<v8::Context> context, AST::SourceLocation *impl);
    static AST::SourceLocation *impl(v8::Local<v8::Object> object);

    static v8::Local<v8::FunctionTemplate> constructorTemplate(v8::Isolate *isolate);

    static void toStringCallback(const v8::FunctionCallbackInfo<v8::Value> &info);
    static void lineGetterCallback(const v8::FunctionCallbackInfo<v8::Value> &info);
    static void columnGetterCallback(const v8::FunctionCallbackInfo<v8::Value> &info);
    static void offsetGetterCallback(const v8::FunctionCallbackInfo<v8::Value> &info);
    static void fileGetterCallback(const v8::FunctionCallbackInfo<v8::Value> &info);
    ;

    static void breakpoint(v8::Isolate *isolate, const char *source, v8::Local<v8::Name> name = v8::Local<v8::Name>(),
        v8::Local<v8::Value> value = v8::Local<v8::Value>());
};
} // namespace bridge

#endif // SRC_NRD_NBP_V8_BRIDGE_V8_SOURCELOCATION_H_
