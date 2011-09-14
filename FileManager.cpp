#include "FileManager.h"

FileManager::FileManager()
    : QThread(), mFilesLock(QReadWriteLock::Recursive)
{
    setObjectName("FileManager");
    moveToThread(this);
    QSettings settings(QSettings::IniFormat, QSettings::UserScope,
                       QCoreApplication::organizationName());
    QByteArray cached = settings.value("cachedGccArguments").toByteArray();
    if (!cached.isEmpty()) {
        qDebug() << "got" << cached.size() << "of cache";
        QDataStream ds(&cached, QIODevice::ReadOnly);
        ds >> mFiles;
        for (QHash<Path, GccArguments>::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it) {
            qDebug() << it.key() << it.value();
        }

        qDebug() << "got" << mFiles.size() << "items";
    }
}
FileManager::~FileManager()
{
    store();
}

FileManager *FileManager::instance()
{
    static QMutex mutex;
    static QWeakPointer<FileManager> ptr;
    if (!ptr) {
        ptr = new FileManager;
    }
    return ptr.data();
}

void FileManager::addMakefile(const Path &makefile)
{
    Q_ASSERT(makefile.exists());
    QCoreApplication::postEvent(this, new FileManagerEvent(FileManagerEvent::MakefileEvent, makefile));
}

GccArguments FileManager::arguments(const Path &path, bool *ok) const
{
    QReadLocker lock(&mFilesLock);
    if (ok)
        *ok = mFiles.contains(path);
    return mFiles.value(path);
}

void FileManager::setArguments(const Path &path, const GccArguments &args)
{
    QWriteLocker lock(&mFilesLock);
    mFiles[path] = args;
    qDebug() << "setting" << path << mFiles.value(path).raw();
}

void FileManager::clearArguments(const Path &path)
{
    QWriteLocker lock(&mFilesLock);
    mFiles.remove(path);
}

bool FileManager::event(QEvent *event)
{
    if (event->type() == int(FileManagerEvent::MakefileEvent)) {
        const Path &path = static_cast<FileManagerEvent*>(event)->path();
        const Path workingDir = path.parentDir();
        Q_ASSERT((path.isFile() && path.isResolved() && workingDir.isDir()) || !path.exists());
        if (!path.exists())
            return true;
        QDir::setCurrent(workingDir); // ### hmmmm
        QProcess *proc = new QProcess(this);
        proc->setWorkingDirectory(workingDir);
        // proc->moveToThread(this);
        connect(proc, SIGNAL(finished(int)), this, SLOT(onMakeFinished(int)));
        connect(proc, SIGNAL(error(QProcess::ProcessError)), this, SLOT(onMakeError(QProcess::ProcessError)));
        connect(proc, SIGNAL(readyReadStandardOutput()), this, SLOT(onMakeOutput()));
        MakefileData data = { path, workingDir, QByteArray(), QSet<Path>(), workingDir };
        mMakefiles[proc] = data;
        proc->start(QLatin1String("make"), // some way to specify which make to use?
                    QStringList()
                    << QLatin1String("-B")
                    << QLatin1String("-n")
                    << QLatin1String("-f")
                    // << QLatin1String("-j1") // ### why doesn't this work?
                    << path);
        qDebug() << "addMakefile" << path;
        return true;
    }
    return QThread::event(event);
}

void FileManager::onMakeFinished(int statusCode)
{
    QProcess *proc = qobject_cast<QProcess*>(sender());
    if (statusCode != 0) {
        qWarning("make error: %s", proc->readAllStandardError().constData());
    }
    mMakefiles.remove(proc);
    proc->deleteLater();
    store();
}

void FileManager::onMakeOutput()
{
    QProcess *proc = qobject_cast<QProcess*>(sender());
    MakefileData &data = mMakefiles[proc];
    int i = data.buffer.size();
    data.buffer += proc->readAllStandardOutput();
    int last = 0;
    const int size = data.buffer.size();
    while (i < size) {
        if (data.buffer.at(i++) == '\n') {
            const QByteArray line = QByteArray::fromRawData(data.buffer.constData() + last,
                                                            i - last - 1);
            last = i;
            // printf("%s\n", line.constData());
            if (!line.isEmpty()) {
                if (line.startsWith("cd ")) {
                    const int slash = line.indexOf('/', 3);
                    if (slash == -1) {
                        qWarning("Can't parse this line. Seems like a cd but no slash [%s]",
                                 line.constData());
                        continue;
                    }
                    Path p = line.mid(3, slash - 3);
                    if (!p.resolve(data.directory)) {
                        qWarning("Can't resolve directory %s with %s",
                                 line.mid(3, slash - 3).constData(),
                                 data.directory.constData());
                    } else {
                        if (Options::s_verbose) {
                            qDebug() << "setting workingDirectory to" << p << "from"
                                     << data.workingDirectory;
                        }
                        data.workingDirectory = p;
                    }
                    continue;
                }

                GccArguments args;
                if (!args.parse(line, data.workingDirectory)) {
                    qWarning("Can't parse line %s (%s)", line.constData(),
                             qPrintable(args.errorString()));
                    continue;
                }

                if (!args.hasInput() || !args.isCompile()) {
                    // if (line.contains(".cpp")) {
                    //     qWarning("No input here or maybe this isn't a compile %d %d %s",
                    //              args.hasInput(), args.isCompile(),
                    //              line.constData());
                    //     if (args.input().size() > 1) {
                    //         qWarning() << args.input();
                    //     }
                    // }
                    continue;
                }

                foreach(const Path &file, args.input()) { // already resolved
                    Q_ASSERT(file.exists());
                    if (!data.seen.contains(file)) {
                        data.seen.insert(file);
                        // qDebug() << "settings arguments for" << file << "to" << args.raw();
                        setArguments(file, args);
                    }
                }
            }
        }
    }
    if (i < size) {
        data.buffer.remove(0, last);
    } else {
        data.buffer.clear();
    }
}
    
void FileManager::onMakeError(QProcess::ProcessError error)
{
    qWarning() << error << qobject_cast<QProcess*>(sender())->errorString();
}

void FileManager::store()
{
    QSettings settings(QSettings::IniFormat, QSettings::UserScope,
                       QCoreApplication::organizationName());

    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << mFiles;
    }
    qDebug() << "writing" << out.size() << "of cache";
    settings.setValue("cachedGccArguments", out);
}
