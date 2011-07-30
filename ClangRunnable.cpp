#include "ClangRunnable.h"

bool ClangRunnable::s_aborted = false;

ClangRunnable::ClangRunnable(const QByteArray &absoluteFilePath,
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

void ClangRunnable::abort()
{
    s_aborted = true;
}

void ClangRunnable::run()
{
    if (s_aborted)
        return;

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
