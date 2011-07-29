#include "PreCompile.h"
#include <QFileInfo>
#include <QDebug>
#include <clang-c/Index.h>
#include <string.h>

#define PRECOMPILE_SEEN_TRESHOLD 3

QString PreCompile::s_path;
QHash<QByteArray, PreCompile*> PreCompile::s_precompiles;

void PreCompile::setPath(const QString &path)
{
    s_path = path;
}

PreCompile* PreCompile::get(const QList<QByteArray> &args)
{
    QByteArray key;
    int skip = 0;
    foreach(const QByteArray& a, args) {
        // not cool
        if (a == "-include-pch")
            skip = 2;
        if (skip && skip--)
            continue;
        key += a + " ";
    }
    QHash<QByteArray, PreCompile*>::const_iterator it = s_precompiles.find(key);
    if (it == s_precompiles.end()) {
        PreCompile* pre = new PreCompile(args);
        s_precompiles[key] = pre;
        return pre;
    }
    return it.value();
}

void PreCompile::clear()
{
    foreach(PreCompile* pre, s_precompiles) {
        delete pre;
    }
    s_precompiles.clear();
}

PreCompile::PreCompile(const QList<QByteArray> &args)
{
    foreach(const QByteArray& arg, args) {
        m_args.append(::strndup(arg.constData(), arg.size()));
    }
    m_args << strdup("-x") << strdup("c++");

    // ### not very safe, files could still be overwritten

    int cnt = 0;
    bool ok = false;
    QString fn;
    do {
        fn = s_path + QString("/rtags_header%1.pch").arg(cnt);
        QFileInfo info(fn);
        ok = !info.exists();
        ++cnt;
    } while (!ok);
    m_filename = fn;

    QFile file(m_filename);
    if (!file.open(QFile::WriteOnly | QFile::Truncate))
        return;
    file.close();

    m_filename = fn;
}

PreCompile::~PreCompile()
{
    foreach(char* arg, m_args) {
        ::free(arg);
    }
    QFile file(m_filename);
    file.remove();
}

QString PreCompile::filename() const
{
    if (m_included.isEmpty())
        return QString();
    return m_filename;
}

void PreCompile::add(const QList<QByteArray> &headers, const QList<QByteArray> &all)
{
    const bool needed = m_seen.isEmpty() && m_included.isEmpty();
    foreach(const QByteArray& header, headers) {
        if (header.isEmpty() || m_included.contains(header))
            continue;
        ++m_seen[header];
    }
    m_seenAll += all.toSet();
    if (!m_seen.isEmpty())
        precompileIfNeeded(needed);
}

void PreCompile::precompileIfNeeded(bool needed)
{
    QByteArray inc;
    QSet<QByteArray> included;
    int max = 0;

    QHash<QByteArray, int>::const_iterator it = m_seen.begin();
    QHash<QByteArray, int>::const_iterator itend = m_seen.end();
    while (it != itend) {
        inc += "#include <" + it.key() + ">\n";
        included.insert(it.key());
        if (it.value() > max)
            max = it.value();
        ++it;
    }

    if (needed || max >= PRECOMPILE_SEEN_TRESHOLD) {
        compile(inc);
        m_seen.clear();
        m_included += included + m_seenAll;
        m_seenAll.clear();
    }
}

void PreCompile::compile(const QByteArray headers)
{
    m_headers += headers;

    QByteArray outfile = m_filename.toLocal8Bit();
    QByteArray infile = outfile + ".h";
    /*
    CXUnsavedFile unsaved;
    unsaved.Filename = infile.constData();
    unsaved.Contents = m_headers.constData();
    unsaved.Length = m_headers.size();
    */
    QFile inp(infile);
    inp.open(QFile::WriteOnly);
    inp.write(m_headers);
    inp.flush();
    inp.close();
    CXIndex idx = clang_createIndex(0, 0);
    //qDebug() << "trying to precompile" << m_headers << "with" << m_args;
    CXTranslationUnit unit = clang_parseTranslationUnit(idx, infile.constData(), m_args.data(), m_args.size(),
                                                        0, 0, CXTranslationUnit_Incomplete);
    //inp.remove();
    if (!unit) {
        clang_disposeIndex(idx);
        return;
    }
    for (unsigned int i = 0; i < clang_getNumDiagnostics(unit); ++i) {
        CXDiagnostic diag = clang_getDiagnostic(unit, i);
        CXString diagstr = clang_getDiagnosticSpelling(diag);
        qDebug() << clang_getCString(diagstr);
        clang_disposeString(diagstr);
        clang_disposeDiagnostic(diag);
    }
    int res = clang_saveTranslationUnit(unit, outfile.constData(), 0);
    qDebug() << "saved?" << res;
    clang_disposeTranslationUnit(unit);
    clang_disposeIndex(idx);
}
