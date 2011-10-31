#include "Precompile.h"
#include <clang-c/Index.h>
#include <QFile>
#include <QFileInfo>
#include <QProcess>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

Path Precompile::s_path;
QHash<QByteArray, Precompile*> Precompile::s_precompiles;

static inline QByteArray keyFromArguments(const GccArguments& args)
{
    QByteArray key;
    foreach(const QByteArray& entry, args.arguments("-D")) {
        key += entry;
    }
    foreach(const QByteArray& entry, args.arguments("-I")) {
        key += entry;
    }

    Q_ASSERT(!key.isEmpty());

    return key;
}

static inline bool writeFile(const QByteArray& filename, const QByteArray& data)
{
    QFile f(filename);
    if (!f.open(QFile::WriteOnly))
        return false;
    const qint64 len = f.write(data);
    return (len == data.size());
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
    : QObject(parent), m_args(args)
{
}

Precompile::~Precompile()
{
    clear();
}

void Precompile::clear()
{
    if (!m_filename.isEmpty()) {
        // removeFile(m_filename);
        // removeFile(m_filename + ".h");
        m_filename.clear();
        m_data.clear();
    }
}

Precompile* Precompile::precompiler(const GccArguments& args)
{
    Q_ASSERT(args.isCompile());

    const QByteArray key = keyFromArguments(args);
    Q_ASSERT(!key.isEmpty());
    const QHash<QByteArray, Precompile*>::const_iterator it = s_precompiles.find(key);
    if (it != s_precompiles.end())
        return it.value();

    Precompile* compile = new Precompile(args);
    s_precompiles[key] = compile;
    return compile;
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

static inline bool preprocessHeaders(QByteArray& headerData, const GccArguments& args, QList<QByteArray> systemIncludes)
{
    Q_ASSERT(args.isCompile() && !args.input().isEmpty());
    Q_ASSERT(!headerData.isEmpty());

    const QList<QByteArray> includePaths = args.arguments("-I");
    QStringList procArgs;
    procArgs << "-E" << "-"
            << byteArrayListToStringList(includePaths)
            << byteArrayListToStringList(systemIncludes)
            << byteArrayListToStringList(args.arguments("-D"))
            << QLatin1String("-x") << QLatin1String(GccArguments::languageString(args.language()));

    QProcess process;
    process.start(QLatin1String("clang"), procArgs);
    process.write(headerData);
    process.closeWriteChannel();
    process.waitForFinished();
    headerData.clear();

    //qDebug() << procArgs;

    const Path sourceFileDir = args.input().front().parentDir();
    const QList<QByteArray> *lists[] = { &includePaths, &systemIncludes };
    foreach(QByteArray line, process.readAllStandardOutput().split('\n')) {
        // qDebug() << mSourceFile << unsaved << line;
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
                const Path resolved = Path::resolved(line, sourceFileDir);
                if (resolved.isHeader()) {
                    headerData += "#include <" + resolved + ">\n";
                    continue;
                }
            }
            enum { Found, DidntFind, DidntWant } state = DidntWant;
            for (int i=0; i<2 && state == DidntWant; ++i) {
                Path dir;
                foreach(const QByteArray &listEntry, *lists[i]) {
                    dir = listEntry.mid(2);
                    dir.resolve(sourceFileDir);
                    const Path resolved = Path::resolved(line, dir);
                    switch (resolved.magicType()) {
                    case Path::Header:
                        headerData += "#include <" + resolved + ">\n";
                        state = Found;
                        break;
                    case Path::Source:
                        state = DidntWant;
                    default:
                        break;
                    }
                }
            }
            if (state == DidntFind) {
                qWarning() << "Couldn't resolve" << line << includePaths << systemIncludes;
            }
        }
    }

    return !headerData.isEmpty();
}

QList<Precompile*> Precompile::precompiles()
{
    return s_precompiles.values();
}

CXTranslationUnit Precompile::precompile(const QList<QByteArray>& systemIncludes, CXIndex idx)
{
    if (m_data.isEmpty())
        return 0;

    if (m_filename.isEmpty()) {
        m_filename = s_path + "/rtagspch_XXXXXX";
        m_filename = "/tmp/rtagspch_XXXXXX";

        int fd = mkstemp(m_filename.data());
        if (fd == -1) {
            fprintf(stderr, "precompile failed to open tempfile %s\n",
                    m_filename.constData());
            m_filename.clear();
            return 0;
        }
        close(fd);
    }
    Q_ASSERT(!m_filename.isEmpty());

    const QByteArray headerFilename = m_filename + ".h";

    // qDebug() << "about to preprocess for pch" << m_data;
    if (!preprocessHeaders(m_data, m_args, systemIncludes)) {
        fprintf(stderr, "failed to preprocess headers for pch\n");
        clear();
        return 0;
    }
    // qDebug() << "done preprocessing for pch" << m_data;

    if (!writeFile(headerFilename, m_data)) {
        fprintf(stderr, "precompile failed to write header file '%s'\n", headerFilename.constData());
        clear();
        return 0;
    }
    printf("Wrote pch header %s\n", headerFilename.constData());

    removeFile(m_filename);

    QVector<const char*> clangArgs;
    clangArgs << "-x" << GccArguments::languageString(m_args.language());
    QList<QByteArray> defines = m_args.arguments("-D"), includes = m_args.arguments("-I");
    foreach(const QByteArray& arg, defines)
        clangArgs << arg.constData();
    foreach(const QByteArray& arg, includes)
        clangArgs << arg.constData();
    foreach(const QByteArray& arg, systemIncludes)
        clangArgs << arg.constData();
    //qDebug() << "about to pch" << m_filename << clangArgs;

    CXTranslationUnit unit = clang_parseTranslationUnit(idx, headerFilename.constData(),
                                                        clangArgs.data(), clangArgs.size(), 0, 0,
                                                        CXTranslationUnit_Incomplete
                                                        | CXTranslationUnit_DetailedPreprocessingRecord);
    if (!unit) {
        fprintf(stderr, "unable to parse pch\n");
        clear();
        return 0;
    }
    int saved = clang_saveTranslationUnit(unit, m_filename.constData(), clang_defaultSaveOptions(unit));
    if (saved != CXSaveError_None) {
        fprintf(stderr, "unable to save pch\n");
        clear();
        printDiagnostics(unit);
        clang_disposeTranslationUnit(unit);
        return 0;
    }
    return unit;
}

QByteArray Precompile::filename() const
{
    return m_filename;
}

void Precompile::addData(const QByteArray& data)
{
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

void Precompile::init(const Path &path)
{
    s_path = path;
}
