#include "TemporaryFiles.h"
#include <QMutexLocker>

TemporaryFiles* TemporaryFiles::s_instance = 0;

TemporaryFiles::TemporaryFiles()
{
}

TemporaryFiles* TemporaryFiles::instance()
{
    if (!s_instance)
        s_instance = new TemporaryFiles;
    return s_instance;
}

void TemporaryFiles::addFile(const QByteArray &filename, const QByteArray &content)
{
    QMutexLocker locker(&m_mutex);
    m_files[filename] = content;
}

void TemporaryFiles::removeFile(const QByteArray &filename)
{
    QMutexLocker locker(&m_mutex);
    m_files.remove(filename);
}

QByteArray TemporaryFiles::content(const QByteArray &filename) const
{
    QMutexLocker locker(&m_mutex);
    return m_files.value(filename);
}

QVector<TemporaryFile> TemporaryFiles::unsavedFiles() const
{
    QVector<TemporaryFile> unsaved;

    QMutexLocker locker(&m_mutex);

    QHash<QByteArray, QByteArray>::const_iterator it = m_files.begin();
    const QHash<QByteArray, QByteArray>::const_iterator end = m_files.end();
    while (it != end) {
        unsaved.append(TemporaryFile());

        TemporaryFile& tempref = unsaved.last();

        tempref.tempFilename = it.key();
        tempref.tempContent = it.value();

        tempref.Filename = tempref.tempFilename.constData();
        tempref.Contents = tempref.tempContent.constData();
        tempref.Length = tempref.tempContent.size();

        ++it;
    }

    return unsaved;
}
