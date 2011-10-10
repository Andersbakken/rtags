#ifndef ClangArgs_h
#define ClangArgs_h

#include <QtCore>
#include "GccArguments.h"
#include "Shared.h"

struct ClangArgs {
    QVector<const char*> clangArgs;
    GccArguments::Language language;
    QSet<QByteArray> compilerSwitches;
    Path pchFile;
    inline void fillClangArgs()
    {
        int size = compilerSwitches.size();
        if (language != GccArguments::LangUndefined)
            size += 2;
        if (!pchFile.isEmpty())
            size += 2;
        clangArgs.resize(size);
        int i = 0;
        foreach(const QByteArray &arg, compilerSwitches) {
            clangArgs[i++] = arg.constData();
        }
        if (language != GccArguments::LangUndefined) {
            clangArgs[i++] = "-x";
            clangArgs[i++] = GccArguments::languageString(language);
        }
        if (!pchFile.isEmpty()) { // ### this has to be last
            clangArgs[i++] = "-include-pch";
            // qDebug() << pchFile << GccArguments::languageString(language);
            clangArgs[i++] = pchFile.constData();
        }
    }

    inline QByteArray toString(const char *file) const
    {
        QByteArray out;
        out.reserve(256);
        out += QUOTE(CLANG_EXECUTABLE);
        if (language == GccArguments::LangCPlusPlus)
            out += "++";
        out += ' ';
        for (int i=0; i<clangArgs.size(); ++i) {
            out += ' ';
            out += clangArgs.at(i);
        }
        if (file) {
            out += ' ';
            out += file;
        }
        return out;
    }
};

#endif
