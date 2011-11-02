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
    static Precompile* precompiler(const GccArguments& args);
    static void cleanup();
    static QList<Precompile*> precompiles();    

    ~Precompile();

    void clear();
    void addData(const QByteArray& data);
    CXTranslationUnit precompile(const QList<QByteArray>& systemIncludes, CXIndex idx);

    QByteArray filePath() const;
    QByteArray headerFilePath() const;
private:
    Precompile(const GccArguments& args, QObject* parent = 0);

    QByteArray m_filePath, m_headerFilePath;
    QByteArray m_data;
    GccArguments m_args;

    static QHash<QByteArray, Precompile*> s_precompiles;
    static Path s_path;
};

#endif // PRECOMPILE_H
