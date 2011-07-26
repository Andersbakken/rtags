#ifndef ClangJob_h
#define ClangJob_h

#include <QtCore>
#include <clang-c/Index.h>
#include "Utils.h"
#include "ThreadPool.h"

class ClangJob : public QObject, public ThreadPoolJob
{
    Q_OBJECT;
public:
    ClangJob(const QString &absoluteFilePath,
             unsigned options,
             const QList<QByteArray> &compilerOptions,
             CXIndex index)
        : m_absoluteFilePath(absoluteFilePath),
          m_options(options),
          m_compilerOptions(compilerOptions),
          m_index(index),
          m_reparseUnit(0)
    {
        setObjectName("ClangJob (parse) " + absoluteFilePath);
        // qDebug() << "creating a thread" << objectName();
    }

    ClangJob(CXTranslationUnit unit, const QString &absoluteFilePath)
        : m_absoluteFilePath(absoluteFilePath), m_options(0),
          m_index(0), m_reparseUnit(unit)
    {
        FUNC1(absoluteFilePath);
        setObjectName("ClangJob (reparse) " + absoluteFilePath);
        // qDebug() << "creating a thread" << objectName();
    }

    void execute()
    {
        // Timer timer(__FUNCTION__, objectName(), true);
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
