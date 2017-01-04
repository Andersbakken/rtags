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
#include "selene.h"

#define TO_STR1(x) #x
#define TO_STR(x) TO_STR1(x)

struct UserData {
    List<AST::Cursor> parents;
    AST *ast;
};
CXChildVisitResult AST::visitor(CXCursor cursor, CXCursor, CXClientData u)
{
    UserData *userData = reinterpret_cast<UserData*>(u);
    assert(userData);
    Cursor::Data *p = userData->parents.isEmpty() ? 0 : userData->parents.back().data.get();
    Cursor c = userData->ast->construct(cursor, p);
    userData->parents.push_back(Cursor { c.data } );
    clang_visitChildren(cursor, visitor, u);
    if (userData->parents.size() > 1)
        userData->parents.pop_back();
    return CXChildVisit_Continue;
}

template <typename T> static void assign(sel::Selector selector, const T &t) { selector = t; }
void assign(sel::Selector selector, const String &str) { selector = str.ref(); }

template <typename T>
static void exposeArray(sel::Selector selector, const std::vector<T> &array)
{
    int i = 0;
    for (const T &t : array) {
        assign(selector[i++], t);
    }
}

static void registerClasses(sel::State &state)
{
    state["SourceLocation"].SetClass<AST::SourceLocation>("line", &AST::SourceLocation::line,
                                                          "column", &AST::SourceLocation::column,
                                                          "file", &AST::SourceLocation::file,
                                                          "offset", &AST::SourceLocation::offset,
                                                          "toString", &AST::SourceLocation::toString);
    state["SourceRange"].SetClass<AST::SourceRange>("start", &AST::SourceRange::start,
                                                    "end", &AST::SourceRange::end,
                                                    "length", &AST::SourceRange::length,
                                                    "toString", &AST::SourceRange::toString);

    auto cursors = state["Cursors"];
    cursors.SetClass<AST::Cursors>("size", &AST::Cursors::size,
                                   "at", &AST::Cursors::at);
    auto cursor = state["Cursor"];
    cursor.SetClass<AST::Cursor>("location", &AST::Cursor::location,
                                 "usr", &AST::Cursor::usr,
                                 "kind", &AST::Cursor::kind,
                                 "linkage", &AST::Cursor::linkage,
                                 "availability", &AST::Cursor::availability,
                                 "language", &AST::Cursor::language,
                                 "spelling", &AST::Cursor::spelling,
                                 "displayName", &AST::Cursor::displayName,
                                 "rawComment", &AST::Cursor::rawComment,
                                 "briefComment", &AST::Cursor::briefComment,
                                 "mangledName", &AST::Cursor::mangledName,
                                 "templateKind", &AST::Cursor::templateKind,
                                 "range", &AST::Cursor::range,
                                 "children", &AST::Cursor::children,
                                 "query", &AST::Cursor::query,
                                 "None", &AST::Cursor::none,
                                 "Add", &AST::Cursor::add,
                                 "Recurse", &AST::Cursor::recurse,
                                 "overriddenCount", &AST::Cursor::overriddenCount,
                                 "overriddenCursors", &AST::Cursor::overriddenCursors,
                                 "argumentCount", &AST::Cursor::argumentCount,
                                 "arguments", &AST::Cursor::arguments,
                                 "fieldBitWidth", &AST::Cursor::fieldBitWidth,
                                 "typedefUnderlyingType", &AST::Cursor::typedefUnderlyingType,
                                 "enumIntegerType", &AST::Cursor::enumIntegerType,
                                 "enumConstantValue", &AST::Cursor::enumConstantValue,
                                 "includedFile", &AST::Cursor::includedFile,
                                 "templateArgumentCount", &AST::Cursor::templateArgumentCount,
                                 "templateArgumentType", &AST::Cursor::templateArgumentType,
                                 "templateArgumentValue", &AST::Cursor::templateArgumentValue,
                                 "templateArgumentKind", &AST::Cursor::templateArgumentKind,
                                 "referenced", &AST::Cursor::referenced,
                                 "canonical", &AST::Cursor::canonical,
                                 "lexicalParent", &AST::Cursor::lexicalParent,
                                 "semanticParent", &AST::Cursor::semanticParent,
                                 "definitionCursor", &AST::Cursor::definitionCursor,
                                 "specializedCursorTemplate", &AST::Cursor::specializedCursorTemplate,
                                 "childCount", &AST::Cursor::childCount,
                                 "child", &AST::Cursor::child,
                                 "isBitField", &AST::Cursor::isBitField,
                                 "isVirtualBase", &AST::Cursor::isVirtualBase,
                                 "isStatic", &AST::Cursor::isStatic,
                                 "isVirtual", &AST::Cursor::isVirtual,
                                 "isPureVirtual", &AST::Cursor::isPureVirtual,
                                 "isConst", &AST::Cursor::isConst,
                                 "isDefinition", &AST::Cursor::isDefinition,
                                 "isDynamicCall", &AST::Cursor::isDynamicCall);
}

std::shared_ptr<AST> AST::create(const Source &source, const String &sourceCode, CXTranslationUnit unit)
{
    std::shared_ptr<AST> ast(new AST);
    ast->mState.reset(new sel::State {true});
    sel::State &state = *ast->mState;
    registerClasses(state);
    ast->mSourceCode = sourceCode;
    state["sourceFile"] = source.sourceFile().ref();
    state["sourceCode"] = sourceCode.ref();
    state["write"] = [ast](const std::string &str) {
        // error() << "writing" << str;
        ast->mReturnValues.append(str);
    };

    exposeArray(state["commandLine"], source.toCommandLine(Source::Default|Source::IncludeCompiler|Source::IncludeSourceFile));

    if (unit) {
        UserData userData;
        userData.ast = ast.get();
        visitor(clang_getTranslationUnitCursor(unit), clang_getNullCursor(), &userData);

        const Cursor root = userData.parents.front();
        state["root"] = [root]() { return root; };
        state["findByUsr"] = [ast](const std::string &usr) {
            return ast->mByUsr.value(usr);
        };

        state["findByOffset"] = [ast](const std::string &str) {
            // int offset = atoi(str.c_str());
            // if (offset) {

            // } else
            // sscanf
            // return mByUsr.value(usr);
        };
        const String script = Path(TO_STR(RTAGS_SOURCE_DIR) "/rtags.lua").readAll();
        state(script.constData());
    }
    return ast;
}

List<AST::Diagnostic> AST::diagnostics() const
{
    return List<Diagnostic>();
}

List<AST::SkippedRange> AST::skippedRanges() const
{
    return List<SkippedRange>();
}

List<String> AST::evaluate(const String &script)
{
    assert(mReturnValues.isEmpty());
    try {
        mState->operator()(script.constData());
    } catch (...) {
        error() << "Got exception";
    }
    return std::move(mReturnValues);
}
