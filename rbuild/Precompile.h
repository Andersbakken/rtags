#ifndef PRECOMPILE_H
#define PRECOMPILE_H

#include "GccArguments.h"
#include <Path.h>
#include <QObject>
#include <QList>
#include <QHash>

class Precompile : public QObject
{
    Q_OBJECT
public:
    static Precompile* precompiler(const GccArguments& args);
    static void cleanup();

    ~Precompile();

    void clear();
    void addHeaders(const QList<Path>& headers);
    void precompile(const QList<QByteArray>& systemIncludes);

    bool isCompiled() const;
    QByteArray filename() const;

private:
    Precompile(const GccArguments& args, QObject* parent = 0);

    bool m_compiled;
    QByteArray m_filename;
    QList<Path> m_headers;
    QHash<Path, int> m_seen;
    GccArguments m_args;

    static QHash<QByteArray, Precompile*> s_precompiles;
};

#endif // PRECOMPILE_H
