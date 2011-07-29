#ifndef PRECOMPILE_H
#define PRECOMPILE_H

#include <QByteArray>
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

    static void setPath(const QString& path);

    void add(const QList<QByteArray>& direct, const QList<QByteArray>& all);
    QString filename() const;

private:
    PreCompile(const QList<QByteArray>& args);

    static QHash<QByteArray, PreCompile*> s_precompiles;
    static QString s_path;

private:
    void precompileIfNeeded(bool needed);
    void compile(const QByteArray headers);

private:
    QByteArray m_headers;
    QVector<char*> m_args;
    QList<QPair<QByteArray, int> > m_seen;
    QSet<QByteArray> m_seenAll;
    QSet<QByteArray> m_included;
    QString m_filename;
};

#endif // PRECOMPILE_H
