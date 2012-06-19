#ifndef GCCARGUMENTS_H
#define GCCARGUMENTS_H

#include "Path.h"
#include <QList>
#include <ByteArray.h>

class GccArgumentsImpl;

class GccArguments
{
public:
    enum Type { NoType, Compile, Pch };
    enum Lang { NoLang, C, CPlusPlus };

    GccArguments();
    GccArguments(const ByteArray &args, const Path &base);
    ~GccArguments();

    bool parse(ByteArray args, const Path &base);
    Type type() const;
    Lang lang() const;
    void clear();

    void addFlags(const QList<ByteArray> &extraFlags);
    QList<ByteArray> clangArgs() const;
    QList<Path> inputFiles() const;
    QList<ByteArray> explicitIncludes() const;
    Path outputFile() const;
    Path baseDirectory() const;
    Path compiler() const;
private:
    Q_DISABLE_COPY(GccArguments);
    GccArgumentsImpl *mImpl;
};

#endif
