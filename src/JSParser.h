/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef JSParser_h
#define JSParser_h

#include <RTags.h>
#include <Location.h>
#include <CursorInfo.h>
#include <rct/Log.h>
#include <rct/Hash.h>
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
