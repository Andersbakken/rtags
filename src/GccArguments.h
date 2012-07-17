#ifndef GCCARGUMENTS_H
#define GCCARGUMENTS_H

#include "Path.h"
#include <List.h>
#include <ByteArray.h>

class GccArgumentsImpl;

class GccArguments
{
public:
    enum Type { NoType, Compile, Pch };
    enum Lang { NoLang, C, CPlusPlus };

    GccArguments();
    GccArguments(const ByteArray &args, const Path &base);

    bool parse(ByteArray args, const Path &base);
    Type type() const;
    Lang lang() const;
    void clear();

    void addFlags(const List<ByteArray> &extraFlags);
    List<ByteArray> clangArgs() const;
    List<Path> inputFiles() const;
    List<Path> unresolvedInputFiles() const;
    List<ByteArray> explicitIncludes() const;
    Path outputFile() const;
    Path baseDirectory() const;
    Path compiler() const;
private:
    List<ByteArray> mClangArgs;
    List<Path> mInputFiles, mUnresolvedInputFiles, mIncludes;
    Path mOutputFile, mBase, mCompiler;
    GccArguments::Type mType;
    GccArguments::Lang mLang;
    friend class MakefileParser;
};

#endif
