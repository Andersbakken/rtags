#ifndef GCCARGUMENTS_H
#define GCCARGUMENTS_H

#include "Path.h"
#include <QList>
#include <QByteArray>

class GccArgumentsImpl;

class GccArguments
{
public:
    enum Type { NoType, Compile, Pch };
    enum Lang { NoLang, C, CPlusPlus };

    GccArguments();
    GccArguments(const QByteArray& args, const Path& base);

    bool parse(QByteArray args, const Path& base);
    Type type() const;
    Lang lang() const;

    QList<QByteArray> clangArgs() const;
    QList<QByteArray> inputFiles() const;
    QList<QByteArray> explicitIncludes() const;
    QByteArray outputFile() const;

    QByteArray baseDirectory() const;

private:
    GccArgumentsImpl* mImpl;
};

#endif
