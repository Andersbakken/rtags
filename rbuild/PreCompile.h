#ifndef PRECOMPILE_H
#define PRECOMPILE_H

#include "Path.h"
#include <QHash>
#include <QSet>
#include <QList>
#include <QVector>

class PreCompile
{
public:
    ~PreCompile();

    static PreCompile* get(const QList<QByteArray>& args);

    static void clearAll();
    static void clear(const QByteArray& header);
    static void clear(const QList<QByteArray>& args);

    static void setPath(const Path& path);

    void add(const QList<Path>& direct, const QList<Path>& all);
    Path filename() const;

private:
    PreCompile(const QList<QByteArray>& args);

    static QHash<QByteArray, PreCompile*> s_precompiles;
    static Path s_path;

private:
    void precompileIfNeeded(bool needed);
    void compile(const QByteArray &headers);

private:
    QByteArray m_headers;
    QVector<char*> m_args;
    QList<QPair<Path, int> > m_seen;
    QSet<Path> m_seenAll;
    QSet<Path> m_included;
    Path m_filename;
};

#endif // PRECOMPILE_H
