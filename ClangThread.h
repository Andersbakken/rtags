#ifndef ClangThread_h
#define ClangThread_h

#include <QtCore>
#include <clang-c/Index.h>
#include "Utils.h"

class ClangThread : public QThread
{
    Q_OBJECT;
public:
    ClangThread(const QString &absoluteFilePath,
                unsigned options,
                const QList<QByteArray> &compilerOptions,
                CXIndex index,
                QObject *parent = 0)
        : QThread(parent), m_absoluteFilePath(absoluteFilePath),
          m_options(options),
          m_compilerOptions(compilerOptions),
          m_index(index),
          m_reparseUnit(0)
    {
        setObjectName("ClangThread (parse) " + absoluteFilePath);
    }

    ClangThread(CXTranslationUnit unit, const QString &absoluteFilePath, QObject *parent = 0)
        : QThread(parent), m_absoluteFilePath(absoluteFilePath), m_options(0),
          m_index(0), m_reparseUnit(unit)
    {
        FUNC1(absoluteFilePath);
        setObjectName("ClangThread (reparse) " + absoluteFilePath);
    }

    void run()
    {
        FUNC;
        if (m_reparseUnit) {
            clang_reparseTranslationUnit(m_reparseUnit, 0, 0, 0);
        } else {
            const int size = m_compilerOptions.size();
            QVarLengthArray<const char*, 32> args(size);
            for (int i=0; i<size; ++i) {
                args[i] = m_compilerOptions.at(i).constData();
            }

            CXTranslationUnit unit = clang_parseTranslationUnit(m_index,
                                                                m_absoluteFilePath.toLocal8Bit().constData(),
                                                                args.constData(), size, 0, 0,
                                                                m_options);
            if (!unit) {
                emit error(m_absoluteFilePath);
            } else {
                emit fileParsed(m_absoluteFilePath, unit);
            }
        }
        deleteLater();
    }
signals:
    void fileReparsed(const QString &absoluteFilePath);
    void error(const QString &absoluteFilePath);
    void fileParsed(const QString &absoluteFilePath, void *unit);
private:
    const QString m_absoluteFilePath;
    const unsigned m_options;
    const QList<QByteArray> m_compilerOptions;
    CXIndex m_index;
    CXTranslationUnit m_reparseUnit;
};

#endif
