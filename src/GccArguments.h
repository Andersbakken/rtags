#ifndef GCCARGUMENTS_H
#define GCCARGUMENTS_H

#include <rct/Path.h>
#include <rct/List.h>
#include <rct/String.h>

class GccArgumentsImpl;

class GccArguments
{
public:
    enum Lang { NoLang, C, CPlusPlus };

    GccArguments();

    bool parse(String args, const Path &base);
    Lang lang() const;
    void clear();

    void addFlags(const List<String> &extraFlags);
    List<String> clangArgs() const;
    List<Path> inputFiles() const;
    List<Path> unresolvedInputFiles() const;
    Path outputFile() const;
    Path baseDirectory() const;
    Path compiler() const;
    Path projectRoot() const;
private:
    List<String> mClangArgs;
    List<Path> mInputFiles, mUnresolvedInputFiles;
    Path mOutputFile, mBase, mCompiler;
    GccArguments::Lang mLang;
    friend class MakefileParser;
    friend class Server;
};

#endif
