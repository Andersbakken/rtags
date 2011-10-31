#ifndef PRECOMPILE_H
#define PRECOMPILE_H

#include "GccArguments.h"
#include <Path.h>
#include <QObject>
#include <QList>

class Precompile : public QObject
{
    Q_OBJECT
public:
    static Precompile* precompiler(const GccArguments& args);
    static void cleanup();
    static void precompileAll(const QList<QByteArray>& systemIncludes);

    ~Precompile();

    void clear();
    void addData(const QByteArray& data);
    void precompile(const QList<QByteArray>& systemIncludes);

    QByteArray filename() const;

private:
    Precompile(const GccArguments& args, QObject* parent = 0);

    QByteArray m_filename;
    QByteArray m_data;
    GccArguments m_args;

    static QHash<QByteArray, Precompile*> s_precompiles;
};

#endif // PRECOMPILE_H
