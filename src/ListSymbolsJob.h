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

#ifndef ListSymbolsJob_h
#define ListSymbolsJob_h

#include "QueryJob.h"
#include "rct/String.h"

template <typename T>
class List;
class QueryMessage;
class ListSymbolsJob : public QueryJob
{
public:
    ListSymbolsJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj);
protected:
    virtual int execute() override;
    Set<String> listSymbolsWithPathFilter(const std::shared_ptr<Project> &project, const List<Path> &paths) const;
    Set<String> listSymbols(const std::shared_ptr<Project> &project) const;
    static bool isImenuSymbol(const Symbol &symbol)
    {
        if (!symbol.isReference() && !clang_isStatement(symbol.kind)) {
            switch (symbol.kind) {
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
            case CXCursor_InclusionDirective:
            case CXCursor_EnumConstantDecl:
            case CXCursor_StringLiteral:
            case CXCursor_IntegerLiteral:
            case CXCursor_FloatingLiteral:
            case CXCursor_ImaginaryLiteral:
            case CXCursor_CharacterLiteral:
                break;
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
            case CXCursor_ClassTemplate:
                if (!symbol.isDefinition())
                    break;
                return true;
            default:
                return true;
            }
        }
        return false;
    }
private:
    String string;
};

#endif
