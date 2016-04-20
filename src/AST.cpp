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

struct UserData {
    List<AST::Cursor::Data*> parents;
    AST *ast;
};
CXChildVisitResult AST::visitor(CXCursor cursor, CXCursor, CXClientData u)
{
    UserData *userData = reinterpret_cast<UserData*>(u);
    assert(userData);
    Cursor::Data *p = userData->parents.isEmpty() ? 0 : userData->parents.back();
    // assert(thread->mCursorClass);
    // auto object = thread->mCursorClass->create();
    Cursor c = userData->ast->construct(cursor, p);
    userData->parents.push_back(c.data.get());
    clang_visitChildren(cursor, visitor, u);
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


std::shared_ptr<AST> AST::create(const Source &source, CXTranslationUnit unit)
{
    std::shared_ptr<AST> ast(new AST);
    if (unit) {
        UserData userData;
        userData.ast = ast.get();
        visitor(clang_getTranslationUnitCursor(unit), clang_getNullCursor(), &userData);

        sel::State state{true};

        state["sourceFile"] = source.sourceFile().ref();
        exposeArray(state["commandLine"], source.toCommandLine(Source::Default|Source::IncludeCompiler|Source::IncludeSourceFile));

        state["SourceLocation"].SetClass<SourceLocation>("line", &SourceLocation::line,
                                                         "column", &SourceLocation::column,
                                                         "file", &SourceLocation::file,
                                                         "offset", &SourceLocation::offset,
                                                         "toString", &SourceLocation::toString);
        state["SourceRange"].SetClass<SourceRange>("start", &SourceRange::start,
                                                   "end", &SourceRange::end,
                                                   "length", &SourceRange::length,
                                                   "toString", &SourceRange::toString);
        state["Cursor"].SetClass<Cursor>("location", &Cursor::location,
                                         "usr", &Cursor::usr,
                                         "kind", &Cursor::kind,
                                         "linkage", &Cursor::linkage,
                                         "availability", &Cursor::availability,
                                         "language", &Cursor::language,
                                         "spelling", &Cursor::spelling,
                                         "displayName", &Cursor::displayName,
                                         "rawComment", &Cursor::rawComment,
                                         "briefComment", &Cursor::briefComment,
                                         "mangledName", &Cursor::mangledName,
                                         "templateKind", &Cursor::templateKind,
                                         "range", &Cursor::range,
                                         "overriddenCount", &Cursor::overriddenCount,
                                         "overridden", &Cursor::overridden,
                                         "argumentCount", &Cursor::argumentCount,
                                         "argument", &Cursor::argument,
                                         "fieldBitWidth", &Cursor::fieldBitWidth,
                                         "typedefUnderlyingType", &Cursor::typedefUnderlyingType,
                                         "enumIntegerType", &Cursor::enumIntegerType,
                                         "enumConstantValue", &Cursor::enumConstantValue,
                                         "includedFile", &Cursor::includedFile,
                                         "templateArgumentCount", &Cursor::templateArgumentCount,
                                         "templateArgumentCursor", &Cursor::templateArgumentCursor,
                                         "templateArgumentValue", &Cursor::templateArgumentValue,
                                         "templateArgumentKind", &Cursor::templateArgumentKind,
                                         "referenced", &Cursor::referenced,
                                         "canonical", &Cursor::canonical,
                                         "lexicalParent", &Cursor::lexicalParent,
                                         "semanticParent", &Cursor::semanticParent,
                                         "definitionCursor", &Cursor::definitionCursor,
                                         "specializedCursorTemplate", &Cursor::specializedCursorTemplate,
                                         "isBitField", &Cursor::isBitField,
                                         "isVirtualBase", &Cursor::isVirtualBase,
                                         "isStatic", &Cursor::isStatic,
                                         "isVirtual", &Cursor::isVirtual,
                                         "isPureVirtual", &Cursor::isPureVirtual,
                                         "isConst", &Cursor::isConst,
                                         "isDefinition", &Cursor::isDefinition,
                                         "isDynamicCall", &Cursor::isDynamicCall);
    }
}

List<AST::Diagnostic> AST::diagnostics() const
{
}

List<AST::SkippedRange> AST::skippedRanges() const
{
}

void AST::evaluate(const String &script)
{
}
