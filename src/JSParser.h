#ifndef JSParser_h
#define JSParser_h

#include <RTags.h>
#include <Location.h>
#include <CursorInfo.h>
#include <rct/Log.h>
#include <rct/Map.h>
#include <rct/Path.h>
#include <rct/String.h>
#include <v8.h>

class JSParser
{
public:
    JSParser();
    ~JSParser();
    bool init();

    bool parse(const Path &path, const String &contents,
               SymbolMap *cursors,
               SymbolNameMap *symbolNames,
               String *ast);
private:
    v8::Persistent<v8::Context> mContext;
    v8::Persistent<v8::Function> mParse;
    v8::Isolate *mIsolate;
};


#endif
