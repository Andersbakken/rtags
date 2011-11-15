#include "Precompile.h"
#include <clang-c/Index.h>
#include <QFile>
#include <QFileInfo>
#include <QProcess>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

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
    return f.write(data) == data.size();
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
        CXString txt = clang_getDiagnosticSpelling(diag);
        fprintf(stderr, "%s\n", clang_getCString(txt));
        clang_disposeString(txt);
        clang_disposeDiagnostic(diag);
    }
}

bool Precompile::preprocessHeaders(QList<QByteArray> systemIncludes)
{
    Q_ASSERT(m_args.isCompile() && !m_args.input().isEmpty());
    Q_ASSERT(!m_data.isEmpty());

    const QList<QByteArray> includePaths = m_args.arguments("-I");
    QStringList procArgs;
    procArgs << "-E" << "-"
             << byteArrayListToStringList(includePaths)
             << byteArrayListToStringList(systemIncludes)
             << byteArrayListToStringList(m_args.arguments("-D"))
             << QLatin1String("-x") << QLatin1String(GccArguments::languageString(m_args.language()));

    QProcess process;
    process.start(QLatin1String("clang"), procArgs);
    process.write(m_data);
    process.closeWriteChannel();
    process.waitForFinished();
    // printf("clang ");
    // foreach(const QString &str, procArgs) {
    //     printf("%s ", qPrintable(str));
    // }
    // printf(" < %s\n", m_data.constData());
    m_data.clear();

    QSet<Path> headers;
    const QSet<Path> sourceFileDirs = m_dataPaths;
    const QList<QByteArray> *lists[] = { &includePaths, &systemIncludes };
    foreach(QByteArray line, process.readAllStandardOutput().split('\n')) {
        if (!line.isEmpty()) {
            bool quote = true;
            switch (line.at(0)) {
            case '"':
                if (line.at(line.size() - 1) != '"') { // flag error?
                    qWarning() << "Weird include" << line;
                    continue;
                }
                break;
            case '<':
                if (line.at(line.size() - 1) != '>') { // flag error?
                    qWarning() << "Weird include" << line;
                    continue;
                }
                quote = false;
                break;
            default:
                continue;
            }
            line.remove(0, 1);
            line.chop(1);
            // qWarning() << "looking for" << line;
            if (quote) {
                Path resolved;
                bool found = false;
                foreach(const Path &sourceDir, sourceFileDirs) {
                    resolved = Path::resolved(line, sourceDir);
                    found = resolved.isFile();
                    if (found)
                        break;
                }
                if (found && !resolved.isSource()) {
                    if (!headers.contains(resolved)) {
                        m_data += "#include <" + resolved + ">\n";
                        headers.insert(resolved);
                    }
                    continue;
                }
            }
            enum { Found, DidntFind, DidntWant } state = DidntFind;
            for (int i=0; i<2 && state == DidntFind; ++i) {
                Path dir;
                foreach(const QByteArray &listEntry, *lists[i]) {
                    dir = listEntry.mid(2);
                    foreach(const Path &sourceFileDir, sourceFileDirs) {
                        if (dir.resolve(sourceFileDir))
                            break;
                    }
                    const Path resolved = Path::resolved(line, dir);
                    if (resolved.isFile()) {
                        if (!resolved.isSource()) {
                            if (!headers.contains(resolved)) {
                                headers.insert(resolved);
                                m_data += "#include <" + resolved + ">\n";
                            }
                            state = Found;
                        } else {
                            state = DidntWant;
                        }
                    }
                }
            }
            // qDebug() << state << line;
            if (state == DidntFind) {
                qWarning() << "Couldn't resolve" << line << includePaths << systemIncludes;
            }
        }
    }

    return !m_data.isEmpty();
}

QList<Precompile*> Precompile::precompiles()
{
    return s_precompiles.values();
}

CXTranslationUnit Precompile::precompile(const QList<QByteArray>& systemIncludes, CXIndex idx)
{
    Q_ASSERT(!m_compiled);
    if (m_data.isEmpty()) {
        return 0;
    }

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

    // qDebug() << "about to preprocess for pch" << m_data;
    if (!preprocessHeaders(systemIncludes)) {
        fprintf(stderr, "failed to preprocess headers for pch\n");
        clear();
        return 0;
    }
    // qDebug() << "done preprocessing for pch" << m_data;

    QVector<const char*> clangArgs;
    clangArgs << "-cc1" << "-x" << m_args.languageString();
    QList<QByteArray> defines = m_args.arguments("-D"), includes = m_args.arguments("-I");
    foreach(const QByteArray& arg, defines)
        clangArgs << arg.constData();
    foreach(const QByteArray& arg, includes)
        clangArgs << arg.constData();
    foreach(const QByteArray& arg, systemIncludes)
        clangArgs << arg.constData();

    // printf("clang ");
    // foreach(const char *arg, clangArgs) {
    //     printf("%s ", arg);
    // }
    // printf("%s\n", headerFilePath().constData());

    //qDebug() << "about to pch" << m_filePath << clangArgs;

    if (!writeFile(headerFilePath(), clangArgs, m_data)) {
        fprintf(stderr, "precompile failed to write header file '%s'\n", headerFilePath().constData());
        clear();
        return 0;
    }
    // printf("Wrote pch header %s\n", headerFilename.constData());

    removeFile(m_filePath);

    CXTranslationUnit unit = clang_parseTranslationUnit(idx, headerFilePath().constData(),
                                                        clangArgs.data(), clangArgs.size(), 0, 0,
                                                        CXTranslationUnit_Incomplete
                                                        | CXTranslationUnit_DetailedPreprocessingRecord);
    if (!unit) {
        fprintf(stderr, "unable to parse pch\n");
        clear();
        return 0;
    }
    int saved = clang_saveTranslationUnit(unit, m_filePath.constData(), clang_defaultSaveOptions(unit));
    if (saved != CXSaveError_None) {
        fprintf(stderr, "unable to save pch\n");
        clear();
        printDiagnostics(unit);
        clang_disposeTranslationUnit(unit);
        return 0;
    }
    m_compiled = true;
    return unit;
}

Path Precompile::filePath() const
{
    return m_filePath;
}

Path Precompile::headerFilePath() const
{
    return m_headerFilePath;
}

void Precompile::addData(const QByteArray& data, const Path &path)
{
    m_dataPaths.insert(path);
    m_data += data;
}

/*
  static inline bool filter(const Path& header)
  {
  if (header.contains("/bits/") || header.endsWith("_p.h") || header.endsWith("_impl.h") || header.contains("/private/"))
  return true;
  return false;
  }
*/
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
