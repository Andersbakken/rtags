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

#ifndef GCCARGUMENTS_H
#define GCCARGUMENTS_H

#include <rct/Path.h>
#include <rct/List.h>
#include <rct/Map.h>
#include <rct/String.h>

class GccArgumentsImpl;

class GccArguments
{
public:
    enum Lang { NoLang, C, CPlusPlus };

    GccArguments();

    bool parse(String args, const Path &base);
    bool parse(List<String> split, const Path &base);

    Lang lang() const;
    void clear();

    void addFlags(const List<String> &extraFlags);
    List<String> clangArgs() const;
    List<Path> inputFiles() const;
    List<Path> unresolvedInputFiles() const;
    Path baseDirectory() const;
    Path compiler() const;
    Path projectRoot() const;
    enum IncludeType {
        INone,
        IQuote,
        ISystem,
        Include
    };
    Map<Path, IncludeType> includes() const;
private:
    List<String> mClangArgs;
    Map<Path, IncludeType> mIncludes;
    List<Path> mInputFiles, mUnresolvedInputFiles;
    Path mBase, mCompiler;
    GccArguments::Lang mLang;
    friend class MakefileParser;
    friend class Server;
};

#endif
