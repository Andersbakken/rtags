/* This file is part of RTags (http://rtags.net).

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

#include "AST.h"
#include "ClangThread.h"
#include <v8.h>
#include <libplatform/libplatform.h>
#include "V8SourceLocation.h"
#include "TypeConverter.h"
#include "V8Cursor.h"

#define TO_STR1(x) #x
#define TO_STR(x) TO_STR1(x)

struct UserData {
    List<std::shared_ptr<Cursor> > parents;
    AST *ast;
};
CXChildVisitResult AST::visitor(CXCursor cursor, CXCursor, CXClientData u)
{
    UserData *userData = reinterpret_cast<UserData*>(u);
    assert(userData);
    std::shared_ptr<Cursor> p = userData->parents.isEmpty() ? nullptr : userData->parents.back();
    std::shared_ptr<Cursor> c = userData->ast->construct(cursor, p);
    userData->parents.push_back(c);
    clang_visitChildren(cursor, visitor, u);
    if (userData->parents.size() > 1)
        userData->parents.pop_back();
    return CXChildVisit_Continue;
}

static void log(const v8::FunctionCallbackInfo<v8::Value> &info)
{
    AST *ast = static_cast<AST *>(v8::Local<v8::External>::Cast(info.Data())->Value());
    assert(ast);
    Log l(&ast->currentOutput());
    for (int i = 0; i < info.Length(); ++i) {
        v8::String::Utf8Value str(info.GetIsolate(), info[i]);
        l << *str;
    }
}

#warning expose dependencies
#warning expose skipped ranges
#warning expose diagnostics

std::shared_ptr<AST> AST::create(const Source &source, const String &sourceCode, CXTranslationUnit unit,
                                 const std::vector<String> &scripts, const std::function<void(const String &)> &outputHandler)
{
    assert(unit);
    std::shared_ptr<AST> ast(new AST);
    const Path exec = Rct::executablePath();
    std::unique_ptr<v8::Platform> platform = v8::platform::NewDefaultPlatform();
    v8::V8::InitializePlatform(platform.get());
    static bool first = true;
    if (first) {
        first = false;
        v8::V8::Initialize();
    }
    v8::Isolate::CreateParams params;
    struct ArrayBufferAllocator : public v8::ArrayBuffer::Allocator
    {
        virtual void* Allocate(size_t length) { return calloc(length, 1); }
        virtual void* AllocateUninitialized(size_t length) { return malloc(length); }
        virtual void Free(void* data, size_t /*length*/) { free(data); }
    } static sArrayBufferAllocator;

    params.array_buffer_allocator = &sArrayBufferAllocator;
    v8::Isolate *isolate = v8::Isolate::New(params);
    v8::Isolate::Scope isolateScope(isolate);
    v8::HandleScope handleScope(isolate);
    V8PerIsolateData::Init(isolate);
    v8::Local<v8::ObjectTemplate> globalTemplate = v8::ObjectTemplate::New(isolate);
    v8::Local<v8::External> astPointer = v8::External::New(isolate, ast.get());
    globalTemplate->Set(isolate, "log", v8::FunctionTemplate::New(isolate, &log, astPointer));
    globalTemplate->Set(isolate, "sourceFile", createV8String(isolate, source.sourceFile()));
    globalTemplate->Set(isolate, "sourceCode", createV8String(isolate, sourceCode));
    v8::Local<v8::Context> context(v8::Context::New(isolate, nullptr, globalTemplate));
    v8::Context::Scope contextScope(context);
    v8::Local<v8::Object> globalObject = context->Global();
    const List<String> cmd = source.toCommandLine(Source::Default|Source::IncludeCompiler|Source::IncludeSourceFile);
    v8::Local<v8::Array> commandLine = v8::Array::New(isolate, cmd.size());
    for (size_t i=0; i<cmd.size(); ++i) {
        commandLine->Set(context, i, createV8String(isolate, cmd.at(i))).IsJust();
    }
    globalObject->Set(context, createV8String(isolate, "commandLine"), commandLine).IsJust();

    String src = Path(TO_STR(RTAGS_SOURCE_DIR) "/rtags.js").readAll();
    {
        v8::Local<v8::Script> script = v8::Script::Compile(context, createV8String(isolate, src.constData())).ToLocalChecked();
        script->Run(context).ToLocalChecked();
    }
    UserData userData;
    userData.ast = ast.get();
    visitor(clang_getTranslationUnitCursor(unit), clang_getNullCursor(), &userData);

    std::shared_ptr<Cursor> root = userData.parents.front();
    if (!ast->mCurrentOutput.isEmpty()) {
        outputHandler(ast->mCurrentOutput);
        ast->mCurrentOutput.clear();
        globalObject->Set(context, createV8String(isolate, "root"), V8Cursor::wrap(isolate, context, root.get()).ToLocalChecked()).ToChecked();
        for (const String &s : scripts) {
            v8::Local<v8::Script> script = v8::Script::Compile(context, createV8String(isolate, s.constData())).ToLocalChecked();
            script->Run(context).ToLocalChecked();
            if (!ast->mCurrentOutput.isEmpty()) {
                outputHandler(ast->mCurrentOutput);
                ast->mCurrentOutput.clear();
            }
        }
    }
    // state["root"] = [root]() { return root; };
    // state["findByUsr"] = [ast](const std::string &usr) {
    //     return ast->mByUsr.value(usr);
    // };

    // state["findByOffset"] = [ast](const std::string &str) {
    //     // int offset = atoi(str.c_str());
    //     // if (offset) {

    //     // } else
    //     // sscanf
    //     // return mByUsr.value(usr);
    // };
    // const String script = Path(TO_STR(RTAGS_SOURCE_DIR) "/rtags.js").readAll();
    // state(script.constData());
    return ast;
}

// List<AST::Diagnostic> AST::diagnostics() const
// {
//     return List<Diagnostic>();
// }

// List<AST::SkippedRange> AST::skippedRanges() const
// {
//     return List<SkippedRange>();
// }

std::shared_ptr<Cursor> AST::construct(const CXCursor &cursor,
                                       const std::shared_ptr<Cursor> &parent,
                                       std::shared_ptr<SourceLocation> loc,
                                       std::string usr) const
{
    std::shared_ptr<Cursor> ret;
    if (!loc)
        loc = createLocation(cursor);
    if (usr.empty())
        usr = toString(clang_getCursorUSR(cursor));
    ret.reset(new Cursor(const_cast<AST*>(this), parent, cursor, loc, usr));
    if (!loc->isNull())
        mByLocation[*loc].push_back(ret);
    if (!usr.empty())
        mByUsr[usr].push_back(ret);
    if (parent)
        parent->mChildren.append(ret);
    return ret;
}
