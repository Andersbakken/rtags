#include "GccArguments.h"
#include "gccopts_gperf.h"

class GccArgumentsImpl
{
public:
    GccArgumentsImpl() : type(GccArguments::Unknown) { }

    QList<QByteArray> clangArgs, inputFiles;
    QList<Path> includes;
    Path outputFile;
    GccArguments::Type type;
    Path base;
};

GccArguments::GccArguments()
    : m_impl(new GccArgumentsImpl)
{
}

GccArguments::GccArguments(const QByteArray& args, const Path& base)
    : m_impl(new GccArgumentsImpl)
{
    parse(args, base);
}

static inline bool isCompiler(const QByteArray& compiler)
{
    QByteArray c;
    const int dash = compiler.lastIndexOf('-');
    if (dash >= 0)
        c = QByteArray::fromRawData(compiler.constData() + dash + 1,
                                    compiler.size() - dash - 1);
    else
        c = QByteArray::fromRawData(compiler.constData(), compiler.size());

    if (c == "g++"
        || c == "gcc"
        || c == "c++"
        || c == "cc")
        return true;
    return false;
}

bool GccArguments::parse(const QByteArray& args, const Path& base)
{
    m_impl->type = Unknown;
    m_impl->clangArgs.clear();
    m_impl->inputFiles.clear();
    m_impl->base = base;

    char quote = '\0';
    QList<QByteArray> split;
    const char* cur = args.constData();
    const char* prev = cur;
    const char* const end = args.constData() + args.size();
    // ### handle escaped quotes?
    while (cur != end) {
        switch (*cur) {
        case '"':
        case '\'':
            if (quote == '\0')
                quote = *cur;
            else if (*cur == quote)
                quote = '\0';
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
        ++cur;
    }
    if (cur > prev)
        split.append(QByteArray(prev, cur - prev));

    if (split.isEmpty())
        return false;

    Path path;
    if (split.front() == "cd" && split.size() > 3 && split.at(2) == "&&") {
        path = Path::resolved(split.at(1), base);
        split.erase(split.begin(), split.begin() + 3);
    } else
        path = base;

    if (!isCompiler(split.front()))
        return false;

    QList<QByteArray> unresolvedInputs;

    bool pathok = false;
    char prevopt = '\1'; // skip the initial binary name
    gccopts_gperf gccopts;
    foreach(const QByteArray& arg, split) {
        cur = arg.constData();
        if (prevopt != '\0') {
            switch (prevopt) {
            case 'x':
                if (!strcmp(cur, "c-header") || !strcmp(cur, "c++-header"))
                    m_impl->type = Pch;
                m_impl->clangArgs.append("-x");
                m_impl->clangArgs.append(cur);
                break;
            case 'i': {
                Path inc = Path::resolved(cur + QByteArray(".gch"), path, &pathok);
                if (!pathok) // try without .gch postfix
                    inc = Path::resolved(cur, path, &pathok);
                if (pathok) {
                    m_impl->includes.append(inc);
                } else {
                    if (!inc.isAbsolute())
                        m_impl->includes.append(Path(path + "/" + cur + QByteArray(".gch"))); // ### is assuming .gch correct here?
                    else
                        qWarning("-include %s could not be resolved", cur);
                } }
                break;
            case 'o': {
                if (!m_impl->outputFile.isEmpty())
                    qWarning("Already have an output file: %s (new %s)",
                             m_impl->outputFile.constData(), cur);
                Path out = Path::resolved(cur, path);
                m_impl->outputFile = out; }
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
                    m_impl->clangArgs.append(cur);
                else if (!strncmp(cur, "-I", 2)) {
                    Path inc = Path::resolved(cur + 2, path, &pathok);
                    if (pathok)
                        m_impl->clangArgs.append("-I" + inc);
                    else
                        qWarning("-I %s could not be resolved", cur + 2);
                }
                else if (m_impl->type == Unknown && !strcmp(cur, "-c"))
                    m_impl->type = Compile;
            }
        } else { // input file?
            Path input = Path::resolved(cur, path, &pathok);
            if (pathok)
                m_impl->inputFiles.append(input);
            else
                unresolvedInputs.append(cur);
        }
    }

    if (m_impl->type == Unknown)
        return false;

    if (m_impl->inputFiles.isEmpty()) {
        qWarning("Unable to find or resolve input files");
        foreach(const QByteArray& input, unresolvedInputs)
            qWarning("  %s", input.constData());
        return false;
    }
    if (m_impl->outputFile.isEmpty() && m_impl->type == Pch) {
        qWarning("Output file is empty for pch");
        return false;
    }
    if (!m_impl->outputFile.isResolved()) {
        if (!m_impl->outputFile.isAbsolute())
            m_impl->outputFile = path + "/" + m_impl->outputFile;
    }

    return true;
}

GccArguments::Type GccArguments::type() const
{
    return m_impl->type;
}

QList<QByteArray> GccArguments::clangArgs() const
{
    return m_impl->clangArgs;
}

QList<QByteArray> GccArguments::inputFiles() const
{
    return m_impl->inputFiles;
}

QList<QByteArray> GccArguments::explicitIncludes() const
{
    QList<QByteArray> incs;
    foreach(const Path& p, m_impl->includes)
        incs.append(p);
    return incs;
}

QByteArray GccArguments::outputFile() const
{
    return m_impl->outputFile;
}

QByteArray GccArguments::baseDirectory() const
{
    return m_impl->base;
}
