#ifndef PRECOMPILE_H
#define PRECOMPILE_H

#include "GccArguments.h"
#include <Path.h>
#include <QObject>
#include <QList>
#include <clang-c/Index.h>

class Precompile : public QObject
{
    Q_OBJECT
public:
    static void create(const GccArguments &args,
                       const Path &pch, const Path &header,
                       const QHash<Path, quint64> &deps);
    static Precompile* precompiler(const GccArguments& args);
    static void cleanup();
    static QList<Precompile*> precompiles();
    static QByteArray pchData();

    ~Precompile();

    void clear();
    void addData(const QByteArray& data, const Path &path);
    CXTranslationUnit precompile(const QList<QByteArray>& systemIncludes, CXIndex idx);

    Path filePath() const;
    Path headerFilePath() const;
    GccArguments arguments() const { return m_args; }
    void setDependencies(const QHash<Path, quint64> &deps) { m_dependencies = deps; }
    QHash<Path, quint64> dependencies() const { return m_dependencies; }
    bool isCompiled() const { return m_compiled; }
private:
    Precompile(const GccArguments& args, QObject* parent = 0);
    bool preprocessHeaders(QList<QByteArray> systemIncludes);

    Path m_filePath, m_headerFilePath;
    QByteArray m_data;
    GccArguments m_args;
    QSet<Path> m_dataPaths;

    QHash<Path, quint64> m_dependencies;

    bool m_compiled;

    static QHash<QByteArray, Precompile*> s_precompiles;
    static Path s_path;
};

#endif // PRECOMPILE_H
