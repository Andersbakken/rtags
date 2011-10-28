#include "Precompile.h"
#include <clang-c/Index.h>
#include <QFile>
#include <QFileInfo>
#include <QProcess>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define SEEN_THRESHOLD 10

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
    : QObject(parent), m_compiled(false), m_args(args)
{
}

Precompile::~Precompile()
{
    clear();
}

void Precompile::clear()
{
    m_compiled = false;
    if (!m_filename.isEmpty()) {
        removeFile(m_filename);
        removeFile(m_filename + ".h");
        m_filename.clear();
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

void Precompile::precompile(const QList<QByteArray>& systemIncludes)
{
    if (m_compiled || m_headers.isEmpty())
        return;

    if (m_filename.isEmpty()) {
        m_filename = "/tmp/rtagspch_XXXXXX";
        int fd = mkstemp(m_filename.data());
        if (fd == -1) {
            fprintf(stderr, "precompile failed to open tempfile\n");
            m_filename.clear();
            return;
        }
        close(fd);
    }
    Q_ASSERT(!m_filename.isEmpty());

    const QByteArray headerFilename = m_filename + ".h";
    QByteArray headerData;
    foreach(const Path& line, m_headers) {
        headerData += "#include <" + line + ">\n";
    }
    if (!writeFile(headerFilename, headerData)) {
        fprintf(stderr, "precompile failed to write header file '%s'\n", headerFilename.constData());
        clear();
        return;
    }

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

    CXIndex idx = clang_createIndex(0, 0);
    CXTranslationUnit unit = clang_parseTranslationUnit(idx, headerFilename.constData(), clangArgs.data(), clangArgs.size(), 0, 0,
                                                        CXTranslationUnit_Incomplete);
    if (!unit) {
        fprintf(stderr, "unable to parse pch\n");
        clear();
        clang_disposeIndex(idx);
        return;
    }
    int saved = clang_saveTranslationUnit(unit, m_filename.constData(), clang_defaultSaveOptions(unit));
    if (saved != CXSaveError_None) {
        fprintf(stderr, "unable to save pch\n");
        clear();
        printDiagnostics(unit);
        clang_disposeTranslationUnit(unit);
        clang_disposeIndex(idx);
        return;
    }
    clang_disposeTranslationUnit(unit);
    clang_disposeIndex(idx);

    m_compiled = true;
}

QByteArray Precompile::filename() const
{
    return m_filename;
}

bool Precompile::isCompiled() const
{
    return m_compiled;
}

static inline bool filter(const Path& header)
{
    if (header.contains("/bits/") || header.endsWith("_p.h") || header.endsWith("_impl.h") || header.contains("/private/"))
        return true;
    return false;
}

static inline bool calcSeen(QHash<Path, int>& seen)
{
    bool recompile = false;
    QHash<Path, int>::iterator it = seen.begin();
    const QHash<Path, int>::const_iterator end = seen.end();
    while (it != end) {
        if (it.value() >= SEEN_THRESHOLD) {
            it.value() = 0;
            recompile = true;
            // don't break here, we need to set all values >= the threshold to 0
        }
        ++it;
    }
    return recompile;
}

void Precompile::addHeaders(const QList<Path>& headers)
{
    const bool wasEmpty = m_headers.isEmpty();
    foreach (const Path& header, headers) {
        if (filter(header))
            continue;
        m_seen[header] += 1;
        if (m_headers.contains(header))
            continue;
        m_headers.append(header);
    }
    m_compiled = !(wasEmpty || calcSeen(m_seen));
}
