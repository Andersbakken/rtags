#ifndef ClangRunnable_h
#define ClangRunnable_h

#include <QtCore>
#include <clang-c/Index.h>
#include "Utils.h"
#include "ThreadPool.h"

class ClangRunnable : public QObject, public QRunnable
{
    Q_OBJECT
public:
    ClangRunnable(const QByteArray &absoluteFilePath,
                  unsigned options,
                  const QList<QByteArray> &compilerOptions,
                  CXIndex index)
        : m_absoluteFilePath(absoluteFilePath),
          m_options(options),
          m_compilerOptions(compilerOptions),
          m_index(index),
          m_reparseUnit(0)
    {
        setAutoDelete(true);
        setObjectName("ClangRunnable (parse) " + absoluteFilePath);
        // qDebug() << "creating a thread" << objectName();
    }

    ClangRunnable(CXTranslationUnit unit, const QByteArray &absoluteFilePath)
        : m_absoluteFilePath(absoluteFilePath), m_options(0),
          m_index(0), m_reparseUnit(unit)
    {
        FUNC1(absoluteFilePath);
        setObjectName("ClangRunnable (reparse) " + absoluteFilePath);
        // qDebug() << "creating a thread" << objectName();
    }

    void run()
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

            QElapsedTimer timer;
            timer.start();
            CXTranslationUnit unit = clang_parseTranslationUnit(m_index,
                                                                m_absoluteFilePath.constData(),
                                                                args.constData(), size, 0, 0,
                                                                m_options);
            qDebug() << "done parsing file" << m_absoluteFilePath << timer.elapsed();
            if (!unit) {
                emit error(m_absoluteFilePath);
            } else {
                emit fileParsed(m_absoluteFilePath, m_compilerOptions, unit);
            }
        }
    }
signals:
    void fileReparsed(const QByteArray &absoluteFilePath);
    void error(const QByteArray &absoluteFilePath);
    void fileParsed(const QByteArray &absoluteFilePath, const QList<QByteArray> &options, void *unit);
private:
    const QByteArray m_absoluteFilePath;
    const unsigned m_options;
    const QList<QByteArray> m_compilerOptions;
    CXIndex m_index;
    CXTranslationUnit m_reparseUnit;
};

#endif
