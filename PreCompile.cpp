#include "PreCompile.h"
#include <QFileInfo>
#include <QDebug>
#include <clang-c/Index.h>
#include <string.h>

#define PRECOMPILE_SEEN_TRESHOLD 10

QString PreCompile::s_path;
QHash<QByteArray, PreCompile*> PreCompile::s_precompiles;

static QByteArray key(const QList<QByteArray>& args)
{
    QByteArray k;
    int skip = 0;
    foreach(const QByteArray& a, args) {
        // not cool
        if (a == "-include-pch")
            skip = 2;
        if (skip && skip--)
            continue;
        k += a + " ";
    }
    return k;
}

void PreCompile::setPath(const QString &path)
{
    s_path = path;
}

PreCompile* PreCompile::get(const QList<QByteArray> &args)
{
    QByteArray key = ::key(args);
    QHash<QByteArray, PreCompile*>::const_iterator it = s_precompiles.find(key);
    if (it == s_precompiles.end()) {
        PreCompile* pre = new PreCompile(args);
        s_precompiles[key] = pre;
        return pre;
    }
    return it.value();
}

void PreCompile::clearAll()
{
    foreach(PreCompile* pre, s_precompiles) {
        delete pre;
    }
    s_precompiles.clear();
}

void PreCompile::clear(const QByteArray &header)
{
    QHash<QByteArray, PreCompile*>::iterator it = s_precompiles.begin();
    while (it != s_precompiles.end()) {
        if (it.value()->m_included.contains(header)) {
            delete it.value();
            it = s_precompiles.erase(it);
        } else
            ++it;
    }
}

void PreCompile::clear(const QList<QByteArray> &args)
{
    QByteArray key = ::key(args);
    QHash<QByteArray, PreCompile*>::iterator it = s_precompiles.find(key);
    if (it != s_precompiles.end()) {
        delete it.value();
        s_precompiles.erase(it);
    }
}

PreCompile::PreCompile(const QList<QByteArray> &args)
{
    foreach(const QByteArray& arg, args) {
        m_args.append(::strdup(arg.constData()));
    }
    m_args << ::strdup("-x") << ::strdup("c++");

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

    QFile file(fn);
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
    QFile file;
    file.setFileName(m_filename);
    file.remove();
    file.setFileName(m_filename + QLatin1String(".h"));
    file.remove();
}

QString PreCompile::filename() const
{
    if (m_included.isEmpty())
        return QString();
    return m_filename;
}

void PreCompile::add(const QList<Path> &headers, const QList<Path> &all)
{
    if (headers.isEmpty())
        return;
    Q_ASSERT(!all.isEmpty());

    const bool needed = m_seen.isEmpty() && m_included.isEmpty();
    foreach(const Path& header, headers) {
        if (header.isEmpty() || m_included.contains(header))
            continue;
        bool found = false;
        QList<QPair<Path, int> >::iterator it = m_seen.begin();
        QList<QPair<Path, int> >::const_iterator itend = m_seen.end();
        while (it != itend) {
            if ((*it).first == header) {
                ++(*it).second;
                found = true;
                break;
            }
            ++it;
        }
        if (!found)
            m_seen.append(QPair<Path, int>(header, 1));
    }
    m_seenAll += all.toSet();
    if (!m_seen.isEmpty())
        precompileIfNeeded(needed);
}

void PreCompile::precompileIfNeeded(bool needed)
{
    QByteArray inc;
    QSet<Path> included;
    int max = 0;

    QList<QPair<Path, int> >::const_iterator it = m_seen.begin();
    QList<QPair<Path, int> >::const_iterator itend = m_seen.end();
    while (it != itend) {
        inc += "#include <" + (*it).first + ">\n";
        included.insert((*it).first);
        if ((*it).second > max) {
            max = (*it).second;
            if (max >= PRECOMPILE_SEEN_TRESHOLD) {
                qDebug() << "re-pch because of" << (*it).first;
            }
        }
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
    QFile inp(infile);
    if (!inp.open(QFile::WriteOnly)) {
        // ### ow!
        m_included.clear();
        return;
    }
    inp.write(m_headers);
    inp.close();
    CXIndex idx = clang_createIndex(0, 0);
    //qDebug() << "trying to precompile" << m_headers << "with" << m_args;
    CXTranslationUnit unit = clang_parseTranslationUnit(idx, infile.constData(), m_args.data(), m_args.size(),
                                                        0, 0, CXTranslationUnit_Incomplete);
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
