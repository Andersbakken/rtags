#ifndef JSParser_h
#define JSParser_h

#include <RTags.h>
#include <Location.h>
#include <CursorInfo.h>
#include <rct/Log.h>
#include <rct/Map.h>
#include <rct/Path.h>
#include <rct/String.h>
#define V8_USE_UNSAFE_HANDLES
#include <v8.h>

class JSParser
{
public:
    JSParser();
    ~JSParser();
    bool init();

    bool parse(const Path &path,
               SymbolMap *cursors,
               SymbolNameMap *symbolNames,
               DependencyMap *dependencies,
               String *ast);
private:
    v8::Persistent<v8::Context> mContext;
    v8::Persistent<v8::Function> mParse;
    v8::Isolate *mIsolate;
};


#endif
