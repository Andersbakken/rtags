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
          m_index(index)
    {
        setAutoDelete(true);
        setObjectName("ClangRunnable (parse) " + absoluteFilePath);
        // qDebug() << "creating a thread" << objectName();
    }

    void run()
    {
        // Timer timer(__FUNCTION__, objectName(), true);
        FUNC;
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
signals:
    void error(const QByteArray &absoluteFilePath);
    void fileParsed(const QByteArray &absoluteFilePath, const QList<QByteArray> &options, void *unit);
private:
    const QByteArray m_absoluteFilePath;
    const unsigned m_options;
    const QList<QByteArray> m_compilerOptions;
    CXIndex m_index;
};

#endif
