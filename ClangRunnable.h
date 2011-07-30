#ifndef ClangRunnable_h
#define ClangRunnable_h

#include <QtCore>
#include <clang-c/Index.h>
#include "Utils.h"

class ClangRunnable : public QObject, public QRunnable
{
    Q_OBJECT
public:
    ClangRunnable(const QByteArray &absoluteFilePath,
                  unsigned options,
                  const QList<QByteArray> &compilerOptions,
                  CXIndex index);
    static void abort();
    void run();
signals:
    void error(const QByteArray &absoluteFilePath);
    void fileParsed(const QByteArray &absoluteFilePath, const QList<QByteArray> &options, void *unit);
private:
    const QByteArray m_absoluteFilePath;
    const unsigned m_options;
    const QList<QByteArray> m_compilerOptions;
    CXIndex m_index;
    static bool s_aborted;
};

#endif
