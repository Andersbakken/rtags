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
#include <rct/Hash.h>
#include <rct/String.h>

class GccArgumentsImpl;

class GccArguments
{
public:
    enum Language { NoLanguage, C, CPlusPlus };

    GccArguments();

    bool parse(String args, const Path &base);
    bool parse(List<String> split, const Path &base);

    Language language() const { return mLanguage; }
    bool isValid() const { return mLanguage != NoLanguage; }
    void clear();

    void addFlags(const List<String> &extraFlags);
    List<String> clangArgs() const { return mClangArgs; }
    List<Path> inputFiles() const { return mInputFiles; }
    List<Path> unresolvedInputFiles() const { return mUnresolvedInputFiles; }
    Path baseDirectory() const { return mBase; }
    Path compiler() const { return mCompiler; }
    Path projectRoot() const;
private:
    List<String> mClangArgs;
    List<Path> mInputFiles, mUnresolvedInputFiles;
    Path mBase, mCompiler;
    GccArguments::Language mLanguage;
    friend class MakefileParser;
    friend class Server;
};

#endif
