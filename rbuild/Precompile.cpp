#include "Precompile.h"
#include <clang-c/Index.h>
#include <QFile>
#include <QFileInfo>
#include <QProcess>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <RTags.h>

using namespace RTags;

QHash<QByteArray, Precompile*> Precompile::s_precompiles;

static inline bool writeFile(const QByteArray& filename, const QVector<const char *> &args,
                             const QByteArray& data)
{
    QFile f(filename);
    if (!f.open(QFile::WriteOnly))
        return false;
    if (f.write("#if 0\n") != 6)
        return false;
    foreach(const char *arg, args) {
        const QByteArray a = QByteArray::fromRawData(arg, strlen(arg));
        if (f.write(a) != a.size())
            return false;
        if (f.write(" ", 1) != 1)
            return false;
    }
    if (f.write("\n#endif\n") != 8)
        return false;
    if (f.write(data) == data.size()) {
        f.flush();
        return true;
    }
    return false;
}

static inline QStringList byteArrayListToStringList(const QList<QByteArray>& input)
{
    QStringList output;
    foreach(const QByteArray& data, input) {
        output.append(QString::fromLocal8Bit(data.constData(), data.size()));
    }

    return output;
}

static inline void removeFile(const QByteArray& filename)
{
    if (filename.isEmpty())
        return;
    QFile f(filename);
    f.remove();
}

static inline bool fileAvailable(const QByteArray& filename)
{
    const QFileInfo f(filename);
    return (f.size() > 0);
}

Precompile::Precompile(const GccArguments& args, QObject* parent)
    : QObject(parent), m_args(args), m_compiled(false)
{
    switch (m_args.language()) {
    case GccArguments::LangC:
        m_args.setLanguage(GccArguments::LangHeader);
        break;
    case GccArguments::LangCPlusPlus:
        m_args.setLanguage(GccArguments::LangCPlusPlusHeader);
        break;
    case GccArguments::LangCPlusPlusHeader:
    case GccArguments::LangHeader:
        break;
    default:
        qWarning("Not sure what to do with this %s %d", args.languageString(), args.language());
        break;
    }
}

Precompile::~Precompile()
{
    clear();
}

void Precompile::clear()
{
    m_compiled = false;
    if (!m_filePath.isEmpty()) {
        // removeFile(m_filePath);
        // removeFile(m_filePath + ".h");
        m_filePath.clear();
        m_data.clear();
    }
}

Precompile* Precompile::precompiler(const GccArguments& args)
{
    Q_ASSERT(args.isCompile());

    const QByteArray key = args.key();
    Q_ASSERT(!key.isEmpty());
    const QHash<QByteArray, Precompile*>::const_iterator it = s_precompiles.find(key);
    if (it != s_precompiles.end())
        return it.value();

    Precompile* compile = new Precompile(args);
    s_precompiles[key] = compile;
    return compile;
}

void Precompile::create(const GccArguments &args,
                        const Path &pch, const Path &header,
                        const QHash<Path, quint64> &deps)
{
    const QByteArray key = args.key();
    Q_ASSERT(!key.isEmpty());
    Precompile* &compile = s_precompiles[key];
    Q_ASSERT(!compile);
    compile = new Precompile(args);
    compile->m_compiled = true;
    compile->m_filePath = pch;
    compile->m_headerFilePath = header;
    compile->m_dependencies = deps;
    s_precompiles[key] = compile;
    // qDebug() << "creating Precompile" << filePath;
}


void Precompile::cleanup()
{
    qDeleteAll(s_precompiles);
    s_precompiles.clear();
}

static inline void printDiagnostics(CXTranslationUnit unit)
{
    unsigned int num = clang_getNumDiagnostics(unit);
    for (unsigned int i = 0; i < num; ++i) {
        CXDiagnostic diag = clang_getDiagnostic(unit, i);
        if (clang_getDiagnosticSeverity(diag) >= CXDiagnostic_Error) {
            CXString txt = clang_getDiagnosticSpelling(diag);
            fprintf(stderr, "%s\n", clang_getCString(txt));
            clang_disposeString(txt);
        }
        clang_disposeDiagnostic(diag);
    }
}

QList<Precompile*> Precompile::precompiles()
{
    return s_precompiles.values();
}

struct FindIncludeOriginUserData {
    QSet<QByteArray> errors;
    int removed;
    QByteArray &data;

    bool recordError(CXFile file)
    {
        // ### ugly
        CXString fileName = clang_getFileName(file);
        const char *fn = clang_getCString(fileName);
        const Path err(fn, strlen(fn));
        // qDebug() << err << errors.contains(err)
        //          << errors << err.fileName();
        const bool ret = fn && errors.contains(err.fileName());
        if (ret) {
            ++removed;
        }

        clang_disposeString(fileName);
        return ret;
    }
};

static inline void findIncludeOrigin(CXFile includedFile,
                                     CXSourceLocation* inclusionStack,
                                     unsigned evilUnsigned,
                                     CXClientData userData)
{
    const int includeLen = evilUnsigned;
    if (includeLen || true) {
        FindIncludeOriginUserData *u = reinterpret_cast<FindIncludeOriginUserData*>(userData);
        bool error = u->recordError(includedFile);
        if (!error) {
            for (int i=0; i<includeLen - 1; ++i) {
                CXFile f;
                clang_getSpellingLocation(inclusionStack[i], &f, 0, 0, 0);
                if (u->recordError(f)) {
                    error = true;
                    break;
                }
            }
        }
        if (error) {
            unsigned o, c;
            clang_getSpellingLocation(inclusionStack[includeLen - 1], 0, 0, &c, &o);
            // qDebug() << "found shit" << u->data.mid(o, 10) << match << u->data
            //          << l << c << o << u->data.size() << eatString(clang_getFileName(file));
            if (u->data.at(o) != ' ') {
                int idx = o - c + 1;
                while (u->data.at(idx) != '\n') {
                    u->data[idx++] = ' ';
                }
            }
        }
    }
}


CXTranslationUnit Precompile::precompile(CXIndex idx)
{
    Q_ASSERT(!m_compiled);
    if (m_data.isEmpty()) {
        return 0;
    }
    const QList<QByteArray> systemIncludes = RTags::systemIncludes();

    if (m_filePath.isEmpty()) {
        m_filePath = "/tmp/rtagspch_pch_XXXXXX";

        int fd = mkstemp(m_filePath.data());
        if (fd == -1) {
            fprintf(stderr, "precompile failed to open tempfile %s\n",
                    m_filePath.constData());
            m_filePath.clear();
            return 0;
        }
        close(fd);
    }
    Q_ASSERT(!m_filePath.isEmpty());

    if (m_headerFilePath.isEmpty()) {
        m_headerFilePath = "/tmp/rtagspch_h_XXXXXX";

        int fd = mkstemp(m_headerFilePath.data());
        if (fd == -1) {
            fprintf(stderr, "precompile failed to open tempfile %s\n",
                    m_headerFilePath.constData());
            m_headerFilePath.clear();
            return 0;
        }
        close(fd);
    }
    Q_ASSERT(!m_headerFilePath.isEmpty());


    QVector<const char*> clangArgs;
    clangArgs << "-cc1" << "-x" << m_args.languageString() << "-ferror-limit=0";
    QList<QByteArray> defines = m_args.arguments("-D"), includes = m_args.arguments("-I");
    foreach(const QByteArray& arg, defines)
        clangArgs << arg.constData();
    foreach(const QByteArray& arg, includes)
        clangArgs << arg.constData();
    foreach(const QByteArray& arg, systemIncludes)
        clangArgs << arg.constData();

    forever {
        {
            QFile f(headerFilePath());
            if (!f.open(QIODevice::WriteOnly) || f.write(m_data) != m_data.size()) {
                fprintf(stderr, "precompile failed to write header file '%s'\n",
                        headerFilePath().constData());
                clear();
                return 0;
            }
        }
        // printf("Wrote pch header %s\n", headerFilename.constData());

        CXTranslationUnit unit = clang_parseTranslationUnit(idx, headerFilePath().constData(),
                                                            clangArgs.data(), clangArgs.size(), 0, 0,
                                                            CXTranslationUnit_Incomplete
                                                            | CXTranslationUnit_DetailedPreprocessingRecord);
        if (!unit) {
            fprintf(stderr, "unable to parse pch\n");
            clear();
            return 0;
        }
        enum {
            Proceed,
            Abort,
            Retry
        } state = Proceed;
        const unsigned int numDiags = clang_getNumDiagnostics(unit);
        QSet<QByteArray> errors;
        for (unsigned int i = 0; i < numDiags; ++i) {
            CXDiagnostic diag = clang_getDiagnostic(unit, i);
            if (clang_getDiagnosticSeverity(diag) >= CXDiagnostic_Error) { // ### error?
                CXSourceLocation loc = clang_getDiagnosticLocation(diag);
                CXFile file;
                unsigned int line, col, off;
                clang_getInstantiationLocation(loc, &file, &line, &col, &off);
                CXString fn = clang_getFileName(file);
                const char* fnstr = clang_getCString(fn);
                if (fnstr) {
                    int lastSlash = -1;
                    int i = 0;
                    forever {
                        if (!fnstr[i]) {
                            break;
                        } else if (fnstr[i] == '/') {
                            lastSlash = i;
                        }
                        ++i;
                    }
                    Q_ASSERT(lastSlash != -1);
                    const QByteArray fileName = QByteArray::fromRawData(fnstr + lastSlash + 1, i - lastSlash - 1);
                    errors.insert(fileName);
                }
                clang_disposeString(fn);
            }
            clang_disposeDiagnostic(diag);
        }
        QSet<QByteArray>::iterator it = errors.begin();
        // qDebug() << errors << m_headerFilePath;
        while (it != errors.end()) {
            const QByteArray errorFile = *it;
            int idx = 0;
            bool found = false;
            while ((idx = m_data.indexOf(errorFile, idx)) != -1) {
                idx += errorFile.size() + 1;
                // qDebug() << errorFile << idx << m_data.mid(idx, 20);
                if (idx + m_data.size() && m_data.at(idx) == '\n') {
                    while (idx > 0) {
                        if (m_data[--idx] == '\n')
                            break;
                        m_data[idx] = ' ';
                        // qDebug() << "erased" << erased;
                    }
                    // qDebug() << "found it setting to retry" << errorFile;
                    found = true;
                    state = Retry;
                }
            }
            if (found) {
                it = errors.erase(it);
            } else {
                ++it;
            }
        }
        if (!errors.isEmpty()) {
            FindIncludeOriginUserData userData = { errors, 0, m_data };
            clang_getInclusions(unit, findIncludeOrigin, &userData);
            if (userData.removed) {
                state = Retry;
            }
        }

        switch (state) {
        case Proceed: {
            removeFile(m_filePath);
            const int saved = clang_saveTranslationUnit(unit, m_filePath.constData(), clang_defaultSaveOptions(unit));
            if (saved != CXSaveError_None) {
                fprintf(stderr, "unable to save pch %s\n", headerFilePath().constData());
                clear();
                printDiagnostics(unit);
                clang_disposeTranslationUnit(unit);
                return 0;
            }
            m_compiled = true;
            // ### should we allow other compiles to use the pch from here or
            // ### should we let them go without pch until it's been properly
            // ### processed?
            return unit; }
        case Retry:
            Q_ASSERT(unit);
            qDebug() << "retrying" << m_headerFilePath;
            clang_disposeTranslationUnit(unit);
            break;
        case Abort:
            if (unit) {
                clang_disposeTranslationUnit(unit);
            }
            return 0;
        }
    }
}

Path Precompile::filePath() const
{
    return m_filePath;
}

Path Precompile::headerFilePath() const
{
    return m_headerFilePath;
}

QByteArray Precompile::pchData()
{
    QByteArray pchData;
    int idx = 0;
    {
        QDataStream ds(&pchData, QIODevice::WriteOnly);
        ds << idx; // will seek back and set to the right value
        foreach(const Precompile *pch, Precompile::precompiles()) {
            if (pch->filePath().isEmpty())
                continue;

            ds << pch->filePath() << pch->headerFilePath() << pch->arguments() << pch->dependencies();
            ++idx;
        }
        ds.device()->seek(0);
        ds << idx;
    }
    if (!idx)
        pchData.clear();
    return pchData;
}
static inline bool filterHeader(const QByteArray &header)
{
    if (header.contains("_win") || header.contains("_symbian"))
        return false;
#ifdef Q_OS_MAC
    if (header.contains("_x11") || header.contains("_qws"))
        return false;
#elif defined Q_OS_UNIX
    if (header.contains("_mac"))
        return false;
#endif
    return true;
}

void Precompile::collectHeaders(const GccArguments &arguments)
{
    QFile file(arguments.input());
    if (!file.open(QIODevice::ReadOnly)) {
        qWarning("Can't open %s for header gathering", arguments.input().constData());
        return;
    }

    enum { MaxLineSize = 1024 };
    char line[1024];
    qint64 read = -1;
    while ((read = file.readLine(line, MaxLineSize)) > 0) {
        if (line[0] == '#') {
            int idx = 1;
            while (isspace(line[idx]))
                ++idx;
            if (strncmp(line + idx, "include", 7))
                continue;
            idx += 7;
            while (isspace(line[idx]))
                ++idx;
            char search;
            switch (line[idx]) {
            case '<':
                search = '>';
                break;
            case '"':
                search = '"';
                break;
            default:
                fprintf(stderr, "I Don't understand this line: %s", line);
                continue;
            }
            int i;
            for (i=idx + 1; i<read; ++i) {
                if (line[i] == search) {
                    // printf("Found include %s\n", Path(line + idx + 1, i - idx - 1).constData());
                    break;
                }
            }
            if (i == read) {
                fprintf(stderr, "I Don't understand this line. Couldn't find %c: %s", search, line);
            } else {
                const QByteArray name = QByteArray::fromRawData(line + idx + 1, i - idx - 1);
                if (filterHeader(name)) {
                    const Path header = arguments.resolve(name, search == '"' ? GccArguments::Quotes : GccArguments::Brackets);
                    if (!header.isFile()) {
                        // qWarning("Can't resolve %s for %s",
                        //          QByteArray(line + idx + 1, i - idx - 1).constData(),
                        //          arguments.input().parentDir().constData());
                    } else if (!header.isSource() && !m_headers.contains(header)) {
                        m_headers.insert(header);
                        m_data += "#include \"";
                        m_data += header;
                        m_data += "\"\n";
                    }
                }
            }

        }
    }
}
