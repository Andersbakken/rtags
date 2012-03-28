#include "GccArguments.h"
#include "gccopts_gperf.h"
#include "Log.h"
#include "RTags.h"

class GccArgumentsImpl
{
public:
    GccArgumentsImpl() : type(GccArguments::NoType), lang(GccArguments::NoLang) { }

    QList<QByteArray> clangArgs, inputFiles;
    QList<Path> includes;
    Path outputFile;
    GccArguments::Type type;
    GccArguments::Lang lang;
    Path base;
};

GccArguments::GccArguments()
    : mImpl(new GccArgumentsImpl)
{
}

GccArguments::GccArguments(const QByteArray& args, const Path& base)
    : mImpl(new GccArgumentsImpl)
{
    parse(args, base);
}

static inline GccArguments::Lang guessLang(const QByteArray& compiler)
{
    QByteArray c;
    const int dash = compiler.lastIndexOf('-');
    if (dash >= 0)
        c = QByteArray::fromRawData(compiler.constData() + dash + 1,
                                    compiler.size() - dash - 1);
    else
        c = QByteArray::fromRawData(compiler.constData(), compiler.size());

    GccArguments::Lang lang = GccArguments::NoLang;
    if (c == "g++"
        || c == "c++")
        lang = GccArguments::CPlusPlus;
    else if (c == "gcc"
             || c == "cc")
        lang = GccArguments::C;
    return lang;
}

bool GccArguments::parse(QByteArray args, const Path& base)
{
    mImpl->type = NoType;
    mImpl->lang = NoLang;
    mImpl->clangArgs.clear();
    mImpl->inputFiles.clear();
    mImpl->base = base;

    char quote = '\0';
    QList<QByteArray> split;
    QByteArray old2 = args;
    {
        char* cur = args.data();
        char* prev = cur;
        // ### handle escaped quotes?
        int size = args.size();
        while (size > 0) {
            switch (*cur) {
            case '\\':
                Q_ASSERT(size > 0);
                memmove(cur, cur + 1, size);
                --size;
                break;

            case '"':
            case '\'':
                if (quote == '\0')
                    quote = *cur;
                else if (*cur == quote)
                    quote = '\0';
                memmove(cur, cur + 1, size);
                --size;
                break;
            case ' ':
                if (quote == '\0') {
                    if (cur > prev)
                        split.append(QByteArray(prev, cur - prev));
                    prev = cur + 1;
                }
                break;
            default:
                break;
            }
            --size;
            ++cur;
        }
        if (cur > prev)
            split.append(QByteArray(prev, cur - prev));
    }

    if (split.isEmpty())
        return false;

    Path path;
    if (split.front() == "cd" && split.size() > 3 && split.at(2) == "&&") {
        path = Path::resolved(split.at(1), base);
        split.erase(split.begin(), split.begin() + 3);
    } else {
        path = base;
    }

    mImpl->lang = guessLang(split.front());
    if (mImpl->lang == NoLang)
        return false;

    QList<QByteArray> unresolvedInputs;

    bool pathok = false;
    char prevopt = '\1'; // skip the initial binary name
    gccopts_gperf gccopts;
    foreach(const QByteArray& arg, split) {
        const char *cur = arg.constData();
        if (prevopt != '\0') {
            switch (prevopt) {
            case 'x':
                if (!strcmp(cur, "c-header")) {
                    mImpl->type = Pch;
                    Q_ASSERT(mImpl->lang == C);
                } else if (!strcmp(cur, "c++-header")) {
                    mImpl->type = Pch;
                    Q_ASSERT(mImpl->lang == CPlusPlus);
                }
                mImpl->clangArgs.append("-x");
                mImpl->clangArgs.append(cur);
                break;
            case 'i': {
                Path inc = Path::resolved(cur + QByteArray(".gch"), path, &pathok);
                if (!pathok) // try without .gch postfix
                    inc = Path::resolved(cur, path, &pathok);
                if (pathok) {
                    mImpl->includes.append(inc);
                } else {
                    if (!inc.isAbsolute())
                        mImpl->includes.append(Path(path + "/" + cur + QByteArray(".gch"))); // ### is assuming .gch correct here?
                    else
                        warning("-include %s could not be resolved", cur);
                } }
                break;
            case 'o': {
                if (!mImpl->outputFile.isEmpty())
                    warning("Already have an output file: %s (new %s)",
                            mImpl->outputFile.constData(), cur);
                Path out = Path::resolved(cur, path);
                mImpl->outputFile = out; }
                break;
            default:
                break;
            }
            prevopt = '\0';
            continue;
        }
        if (!strncmp(cur, "-", 1)) { // option
            if (gccopts.in_word_set(cur, strlen(cur))) {
                if (!strcmp(cur, "-x"))
                    prevopt = 'x';
                else if (!strcmp(cur, "-o"))
                    prevopt = 'o';
                else if (!strcmp(cur, "-include"))
                    prevopt = 'i';
                else
                    prevopt = '\1';
                continue;
            } else {
                if (!strncmp(cur, "-D", 2))
                    mImpl->clangArgs.append(cur);
                else if (!strncmp(cur, "-I", 2)) {
                    const Path inc = Path::resolved(cur + 2, path, &pathok);
                    if (pathok)
                        mImpl->clangArgs.append("-I" + inc);
                }
                else if (mImpl->type == NoType && !strcmp(cur, "-c"))
                    mImpl->type = Compile;
            }
        } else { // input file?
            Path input = Path::resolved(cur, path, &pathok);
            if (pathok)
                mImpl->inputFiles.append(input);
            else
                unresolvedInputs.append(cur);
        }
    }

    if (mImpl->type == NoType)
        return false;

    if (mImpl->inputFiles.isEmpty()) {
        warning("Unable to find or resolve input files");
        foreach(const QByteArray& input, unresolvedInputs)
            warning("  %s", input.constData());
        return false;
    }
    if (mImpl->outputFile.isEmpty() && mImpl->type == Pch) {
        warning("Output file is empty for pch");
        return false;
    }
    if (!mImpl->outputFile.isResolved()) {
        if (!mImpl->outputFile.isAbsolute())
            mImpl->outputFile = path + "/" + mImpl->outputFile;
    }

    return true;
}

GccArguments::Type GccArguments::type() const
{
    return mImpl->type;
}

GccArguments::Lang GccArguments::lang() const
{
    return mImpl->lang;
}

QList<QByteArray> GccArguments::clangArgs() const
{
    return mImpl->clangArgs;
}

QList<QByteArray> GccArguments::inputFiles() const
{
    return mImpl->inputFiles;
}

QList<QByteArray> GccArguments::explicitIncludes() const
{
    QList<QByteArray> incs;
    foreach(const Path& p, mImpl->includes)
        incs.append(p);
    return incs;
}

QByteArray GccArguments::outputFile() const
{
    return mImpl->outputFile;
}

QByteArray GccArguments::baseDirectory() const
{
    return mImpl->base;
}
